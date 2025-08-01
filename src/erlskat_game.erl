%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% Unified game server for trick-taking Skat gameplay
%%% Handles all game types (suit games, grand, null) through card ordering differences
%%% @end
%%% Created : 1 Aug 2025 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_game).

-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/5]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).

%% State callbacks
-export([trick_play/3, game_complete/3]).

-define(SERVER, ?MODULE).

%% State record
-record(state, {
    coordinator_pid :: pid(),
    declarer :: erlskat:player_id(),
    game_type :: binary(),
    players :: [erlskat:player()],
    player_hands :: map(),
    current_trick :: [map()],
    current_leader :: erlskat:player_id(),
    current_player :: erlskat:player_id(),
    player_order :: [erlskat:player_id()],
    tricks_won :: #{erlskat:player_id() => [map()]},
    trick_count :: non_neg_integer(),
    card_ordering :: [erlskat:card()],
    is_hand_game :: boolean(),
    selected_multipliers :: [atom()],
    final_bid :: integer(),
    skat_cards :: erlskat:skat(),
    discarded_cards :: erlskat:cards()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(), erlskat:player_id(), binary(), map(), [erlskat:player()]) ->
          {ok, Pid :: pid()} | ignore | {error, Error :: term()}.
start_link(CoordinatorPid, Declarer, GameType, BiddingResult, Players) ->
    gen_statem:start_link(
      ?MODULE,
      {CoordinatorPid, Declarer, GameType, BiddingResult, Players},
      []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

-spec init({pid(), erlskat:player_id(), binary(), map(), [erlskat:player()]}) ->
          gen_statem:init_result(trick_play).
init({CoordinatorPid, Declarer, GameType, BiddingResult, Players}) ->
    process_flag(trap_exit, true),

    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                declarer => Declarer,
                game_type => GameType,
                players => [maps:get(id, P) || P <- Players],
                action => game_server_starting}),

    % Extract data from bidding result
    PlayerHands = extract_player_hands(maps:get(player_hands, BiddingResult)),
    IsHandGame = maps:get(is_hand_game, BiddingResult, false),
    SelectedMultipliers = maps:get(selected_multipliers, BiddingResult, []),
    FinalBid = maps:get(final_bid, BiddingResult),
    SkatCards = maps:get(skat_cards, BiddingResult),
    DiscardedCards = maps:get(discarded_cards, BiddingResult, []),

    % Get card ordering for this game type
    CardOrdering = erlskat_card_ordering:get_card_ordering_for_game_type(GameType),

    % Set up player order (forehand leads first trick)
    PlayerOrder = get_player_order(Players),
    FirstLeader = hd(PlayerOrder),

    % Initialize tricks won map
    TricksWon = maps:from_list([{maps:get(id, P), []} || P <- Players]),

    % Register game server with manager
    [erlskat_manager:update_player_proc(Player, self()) || Player <- Players],

    % Send initial game state to all players
    broadcast_game_start_to_all_players(
      Players,
      Declarer,
      GameType,
      IsHandGame,
      SelectedMultipliers),

    % Send first trick prompt to first player
    send_card_play_prompt_to_current_player(FirstLeader, PlayerHands, [], CardOrdering, Players),

    InitialState = #state{
        coordinator_pid = CoordinatorPid,
        declarer = Declarer,
        game_type = GameType,
        players = Players,
        player_hands = PlayerHands,
        current_trick = [],
        current_leader = FirstLeader,
        current_player = FirstLeader,
        player_order = PlayerOrder,
        tricks_won = TricksWon,
        trick_count = 0,
        card_ordering = CardOrdering,
        is_hand_game = IsHandGame,
        selected_multipliers = SelectedMultipliers,
        final_bid = FinalBid,
        skat_cards = SkatCards,
        discarded_cards = DiscardedCards
    },

    {ok, trick_play, InitialState}.

%%%===================================================================
%%% State callbacks
%%%===================================================================

%% State: trick_play
trick_play(cast, {socket_message, #{player := Player, msg := CardIndex}}, State) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= State#state.current_player andalso is_integer(CardIndex) of
        true ->
            handle_card_play(PlayerId, CardIndex, State);
        false ->
            keep_state_and_data
    end;

trick_play(EventType, Event, State) ->
    handle_unexpected_event(EventType, Event, State, trick_play).

%% State: game_complete
game_complete(EventType, Event, State) ->
    handle_unexpected_event(EventType, Event, State, game_complete).

-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, _Data) ->
    void.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Extract player hands from bidding result format
extract_player_hands(BiddingHands) ->
    maps:from_list([
        {maps:get(id, maps:get(player, Hand)), maps:get(hand, Hand)}
        || Hand <- BiddingHands
    ]).

%% Determine player order (forehand starts)
get_player_order([Player1, Player2, Player3]) ->
    % Player1 is dealer, so Player2 is forehand (starts)
    [maps:get(id, Player2), maps:get(id, Player3), maps:get(id, Player1)].

%% Handle a card being played
handle_card_play(PlayerId, CardIndex, State) ->
    PlayerHand = maps:get(PlayerId, State#state.player_hands),

    % Validate card index
    case CardIndex >= 0 andalso CardIndex < length(PlayerHand) of
        false ->
            send_invalid_card_error_to_player(PlayerId, State#state.players),
            keep_state_and_data;
        true ->
            PlayedCard = lists:nth(CardIndex + 1, PlayerHand),

            % Validate card play according to Skat rules
            case validate_card_play(
                   PlayedCard,
                   PlayerHand,
                   State#state.current_trick,
                   State#state.game_type) of
                valid ->
                    process_valid_card_play(PlayerId, PlayedCard, CardIndex, State);
                {invalid, Reason} ->
                    send_card_play_error_to_player(PlayerId, Reason, State#state.players),
                    keep_state_and_data
            end
    end.

%% Process a valid card play
process_valid_card_play(PlayerId, PlayedCard, CardIndex, State) ->
    % Update player hand
    PlayerHand = maps:get(PlayerId, State#state.player_hands),
    UpdatedPlayerHand = lists:delete(PlayedCard, PlayerHand),
    UpdatedPlayerHands = maps:put(PlayerId, UpdatedPlayerHand, State#state.player_hands),

    % Add card to current trick
    CardPlay = #{player => PlayerId,
                 card => PlayedCard,
                 position => length(State#state.current_trick)},
    UpdatedCurrentTrick = State#state.current_trick ++ [CardPlay],

    % Broadcast card play to all players
    broadcast_card_played_to_all_players(State#state.players, PlayerId, PlayedCard, CardIndex),

    % Check if trick is complete (3 cards played)
    case length(UpdatedCurrentTrick) of
        3 ->
            complete_trick(State#state{
                player_hands = UpdatedPlayerHands,
                current_trick = UpdatedCurrentTrick
            });
        _ ->
            % Continue to next player
            NextPlayer = get_next_player(PlayerId, State#state.player_order),
            send_card_play_prompt_to_current_player(
              NextPlayer,
              UpdatedPlayerHands,
              UpdatedCurrentTrick,
              State#state.card_ordering,
              State#state.players),

            {keep_state, State#state{
                player_hands = UpdatedPlayerHands,
                current_trick = UpdatedCurrentTrick,
                current_player = NextPlayer
            }}
    end.

%% Complete a trick and determine winner
complete_trick(State) ->
    % Determine trick winner
    TrickWinner = determine_trick_winner(State#state.current_trick, State#state.card_ordering,
                                         State#state.game_type),

    % Add trick to winner's tricks
    WinnerTricks = maps:get(TrickWinner, State#state.tricks_won, []),
    UpdatedTricksWon = maps:put(TrickWinner, [State#state.current_trick | WinnerTricks],
                                State#state.tricks_won),

    % Broadcast trick completion
    broadcast_trick_won_to_all_players(State#state.players, TrickWinner, State#state.current_trick),

    NewTrickCount = State#state.trick_count + 1,

    % Check if game is complete (10 tricks played)
    case NewTrickCount of
        10 ->
            complete_game(State#state{
                tricks_won = UpdatedTricksWon,
                trick_count = NewTrickCount
            });
        _ ->
            % Start next trick with winner leading
            send_card_play_prompt_to_current_player(
              TrickWinner,
              State#state.player_hands,
              [],
              State#state.card_ordering,
              State#state.players),
            {keep_state, State#state{
                current_trick = [],
                current_leader = TrickWinner,
                current_player = TrickWinner,
                tricks_won = UpdatedTricksWon,
                trick_count = NewTrickCount
            }}
    end.

%% Complete the game and calculate results
complete_game(State) ->
    ?LOG_INFO(
       #{module => ?MODULE,
         line => ?LINE,
         function => ?FUNCTION_NAME,
         declarer => State#state.declarer,
         game_type => State#state.game_type,
         action => game_completing}),

    % Calculate game result
    GameResult = calculate_game_result(State),

    % Broadcast game completion to all players
    broadcast_game_complete_to_all_players(State#state.players, GameResult),

    % Send result to coordinator
    CoordinatorPid = State#state.coordinator_pid,
    CoordinatorPid ! {game_complete, self(), GameResult},

    {next_state, game_complete, State, 3000}.

%% Validate card play according to Skat rules
validate_card_play(PlayedCard, PlayerHand, CurrentTrick, GameType) ->
    case CurrentTrick of
        [] ->
            % First card of trick, any card is valid
            valid;
        [FirstPlay | _] ->
            FirstCard = maps:get(card, FirstPlay),
            validate_follow_suit(PlayedCard, FirstCard, PlayerHand, GameType)
    end.

%% Validate following suit rules
validate_follow_suit(PlayedCard, FirstCard, PlayerHand, GameType) ->
    LedSuit = get_effective_suit(FirstCard, GameType),
    PlayedSuit = get_effective_suit(PlayedCard, GameType),

    case PlayedSuit =:= LedSuit of
        true ->
            % Following suit correctly
            valid;
        false ->
            % Check if player has cards of led suit
            HasSuitCards = lists:any(fun(Card) ->
                get_effective_suit(Card, GameType) =:= LedSuit
            end, PlayerHand),

            case HasSuitCards of
                true ->
                    {invalid, <<"Must follow suit">>};
                false ->
                    % No cards of led suit, can play any card
                    valid
            end
    end.

%% Get effective suit considering trumps
get_effective_suit(#{rank := jack}, _GameType) when _GameType =/= <<"null">> ->
    trump;
get_effective_suit(#{suit := Suit}, <<"null">>) ->
    Suit;
get_effective_suit(#{suit := Suit}, GameType) when GameType =:= <<"grand">> ->
    Suit;
get_effective_suit(#{suit := Suit}, GameType) ->
    case GameType of
        <<"clubs">> when Suit =:= clubs -> trump;
        <<"spades">> when Suit =:= spades -> trump;
        <<"hearts">> when Suit =:= hearts -> trump;
        <<"diamonds">> when Suit =:= diamonds -> trump;
        _ -> Suit
    end.

%% Determine trick winner based on card ordering
determine_trick_winner(Trick, CardOrdering, _GameType) ->
    % Create ordering map for quick lookup
    OrderMap = maps:from_list([{Card, Index} || {Index, Card} <- lists:enumerate(CardOrdering)]),

    % Find the highest card played
    {WinningPlay, _} = lists:foldl(fun(Play, {BestPlay, BestValue}) ->
        Card = maps:get(card, Play),
        CardValue = maps:get(Card, OrderMap, 999),
        case CardValue < BestValue of
            true -> {Play, CardValue};
            false -> {BestPlay, BestValue}
        end
    end, {hd(Trick), 999}, Trick),

    maps:get(player, WinningPlay).

%% Calculate final game result
calculate_game_result(State) ->
    % Calculate card points for declarer and defenders
    DeclarerPoints = calculate_player_points(
                       State#state.declarer, State#state.tricks_won,
                       State#state.discarded_cards),
    DefenderPoints = 120 - DeclarerPoints,

    % Determine if declarer won
    DeclarerWon = case State#state.game_type of
        <<"null">> ->
            % In null games, declarer wins by taking 0 tricks
            length(maps:get(State#state.declarer, State#state.tricks_won, [])) =:= 0;
        _ ->
            % In other games, declarer needs >= 61 points
            DeclarerPoints >= 61
    end,

    % Calculate actual game value
    ActualGameValue = calculate_actual_game_value(State, DeclarerWon, DeclarerPoints),
    #{declarer => State#state.declarer,
      declarer_won => DeclarerWon,
      declarer_points => DeclarerPoints,
      defender_points => DefenderPoints,
      game_type => State#state.game_type,
      final_bid => State#state.final_bid,
      actual_game_value => ActualGameValue,
      is_hand_game => State#state.is_hand_game,
      selected_multipliers => State#state.selected_multipliers,
      tricks_won => State#state.tricks_won}.

%% Calculate card points for a player
calculate_player_points(PlayerId, TricksWon, DiscardedCards) ->
    PlayerTricks = maps:get(PlayerId, TricksWon, []),
    TrickCards = lists:flatmap(fun(Trick) ->
        [maps:get(card, Play) || Play <- Trick]
    end, PlayerTricks),
    AllCards = TrickCards ++ DiscardedCards,  % Declarer gets discarded cards
    lists:sum([get_card_points(Card) || Card <- AllCards]).

%% Get point value of a card
get_card_points(#{rank := ace}) -> 11;
get_card_points(#{rank := ten}) -> 10;
get_card_points(#{rank := king}) -> 4;
get_card_points(#{rank := queen}) -> 3;
get_card_points(#{rank := jack}) -> 2;
get_card_points(_) -> 0.

%% Calculate actual game value based on result
calculate_actual_game_value(State, DeclarerWon, DeclarerPoints) ->
    % Use game value module for actual calculation
    PlayerHand = maps:get(State#state.declarer, State#state.player_hands),
    AllDeclarerCards = PlayerHand ++ State#state.discarded_cards,

    % Determine additional bonuses based on points
    IsSchneiderAchieved = DeclarerPoints >= 90 orelse (120 - DeclarerPoints) =< 30,
    IsSchwarzAchieved = DeclarerPoints =:= 120 orelse (120 - DeclarerPoints) =:= 0,

    Options = #{
        is_hand_game => State#state.is_hand_game,
        selected_multipliers => State#state.selected_multipliers,
        is_schneider_achieved => IsSchneiderAchieved,
        is_schwarz_achieved => IsSchwarzAchieved
    },

    Result = erlskat_game_value:calculate_actual_game_value(
               State#state.game_type,
               AllDeclarerCards,
               State#state.skat_cards,
               Options),

    BaseValue = maps:get(value, Result),

    case DeclarerWon of
        true -> BaseValue;
        false -> -2 * BaseValue  % Double penalty for losing
    end.

%% Get next player in order
get_next_player(CurrentPlayer, PlayerOrder) ->
    case lists:dropwhile(fun(P) -> P =/= CurrentPlayer end, PlayerOrder) of
        [_ | [NextPlayer | _]] -> NextPlayer;
        [_] -> hd(PlayerOrder);  % Wrap around to first player
        [] -> hd(PlayerOrder)   % Current player not found, start with first
    end.

%%%===================================================================
%%% Socket messages
%%%===================================================================

%% Send card play prompt to current player
send_card_play_prompt_to_current_player(
  PlayerId,
  PlayerHands,
  CurrentTrick,
  CardOrdering,
  Players) ->
    case find_player_by_id(PlayerId, Players) of
        {ok, Player} ->
            PlayerHand = maps:get(PlayerId, PlayerHands),
            Socket = maps:get(socket, Player),
            Socket ! erlskat_client_responses:card_play_prompt(
                       PlayerHand,
                       CurrentTrick,
                       CardOrdering);
        error ->
            ?LOG_WARNING(#{module => ?MODULE,
                          line => ?LINE,
                          player_id => PlayerId,
                          action => player_not_found_for_prompt})
    end.

%% Broadcast game start to all players
broadcast_game_start_to_all_players(Players, Declarer, GameType, IsHandGame, SelectedMultipliers) ->
    Msg = erlskat_client_responses:game_start_broadcast(
            Declarer,
            GameType,
            IsHandGame,
            SelectedMultipliers),
    [maps:get(socket, Player) ! Msg || Player <- Players].

%% Broadcast card played to all players
broadcast_card_played_to_all_players(Players, PlayerId, PlayedCard, CardIndex) ->
    Msg = erlskat_client_responses:card_played_broadcast(PlayerId, PlayedCard, CardIndex),
    [maps:get(socket, Player) ! Msg || Player <- Players].

%% Broadcast trick won to all players
broadcast_trick_won_to_all_players(Players, WinnerId, Trick) ->
    Msg = erlskat_client_responses:trick_won_broadcast(WinnerId, Trick),
    [maps:get(socket, Player) ! Msg || Player <- Players].

%% Broadcast game complete to all players
broadcast_game_complete_to_all_players(Players, GameResult) ->
    Msg = erlskat_client_responses:game_complete_broadcast(GameResult),
    [maps:get(socket, Player) ! Msg || Player <- Players].

%% Send error messages
send_invalid_card_error_to_player(PlayerId, Players) ->
    case find_player_by_id(PlayerId, Players) of
        {ok, Player} ->
            Socket = maps:get(socket, Player),
            Socket ! erlskat_client_responses:invalid_card_error();
        error ->
            ok
    end.

send_card_play_error_to_player(PlayerId, Reason, Players) ->
    case find_player_by_id(PlayerId, Players) of
        {ok, Player} ->
            Socket = maps:get(socket, Player),
            Socket ! erlskat_client_responses:card_play_error(Reason);
        error ->
            ok
    end.

%% Helper to find player by ID
find_player_by_id(PlayerId, Players) ->
    case lists:filter(fun(P) -> maps:get(id, P) =:= PlayerId end, Players) of
        [Player] -> {ok, Player};
        [] -> error
    end.

%% Handle unexpected events
handle_unexpected_event(EventType, Event, Data, StateName) ->
    ?LOG_WARNING(
       #{module => ?MODULE,
         line => ?LINE,
         function => ?FUNCTION_NAME,
         event_type => EventType,
         event => Event,
         state => StateName,
         action => unhandled_game_event,
         data => Data}),
    keep_state_and_data.
