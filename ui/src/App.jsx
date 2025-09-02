import React, { useState, useEffect, useCallback, useRef } from 'react';
import { useWebSocket } from './utils/websocket';
import LoginScreen from './components/LoginScreen.jsx';
import LobbyScreen from './components/LobbyScreen.jsx';
import GameBoard from './components/GameBoard.jsx';
import PromptModal from './components/PromptModal.jsx';
import './App.css';

const App = () => {
  const [gameState, setGameState] = useState('login'); // login, lobby, game
  const [playerName, setPlayerName] = useState('');
  const [players, setPlayers] = useState([]);
  const [playerHand, setPlayerHand] = useState([]);
  const [leftPlayerCards, setLeftPlayerCards] = useState([]);
  const [rightPlayerCards, setRightPlayerCards] = useState([]);
  const [leftPlayerCardCount, setLeftPlayerCardCount] = useState(0);
  const [rightPlayerCardCount, setRightPlayerCardCount] = useState(0);
  const [currentTrick, setCurrentTrick] = useState([]);
  const [validCards, setValidCards] = useState([]);
  const [prompt, setPrompt] = useState(null);

  // New state for bidding animations
  const [playerId, setPlayerId] = useState(null);
  const [tableOrder, setTableOrder] = useState([]);
  const [currentBidder, setCurrentBidder] = useState(null);
  const [playerBids, setPlayerBids] = useState({});
  const [currentBidValue, setCurrentBidValue] = useState(0);
  const [biddingWinner, setBiddingWinner] = useState(null);

  // New state for card play waiting
  const [currentCardPlayer, setCurrentCardPlayer] = useState(null);

  // New state for game declaration and type broadcasts
  const [gameDeclaration, setGameDeclaration] = useState(null);
  const [gameType, setGameType] = useState(null);

  // New state for discard functionality
  const [discardPrompt, setDiscardPrompt] = useState(null);
  const [selectedDiscardCards, setSelectedDiscardCards] = useState([]);

  // New state for card play functionality
  const [cardPlayPrompt, setCardPlayPrompt] = useState(null);
  const [selectedCardPlay, setSelectedCardPlay] = useState(null);

  // New state for skat cards display
  const [skatCards, setSkatCards] = useState([]);
  const [showSkatCards, setShowSkatCards] = useState(false);

  // New state for player disconnection
  const [disconnectedPlayer, setDisconnectedPlayer] = useState(null);

  // New state for game completion message
  const [gameCompletionMessage, setGameCompletionMessage] = useState(null);

  // New state for player reconnection message
  const [reconnectionMessage, setReconnectionMessage] = useState(null);

  // New state for scores modal
  const [scoresModal, setScoresModal] = useState(null);

  // New state for game info display
  const [gameInfo, setGameInfo] = useState(null);
  const gameInfoRef = useRef(null);

  // New state for card piles (tricks won)
  const [declarerCardsWon, setDeclarerCardsWon] = useState(0);
  const [opponentsCardsWon, setOpponentsCardsWon] = useState(0);

  const handleWebSocketMessage = useCallback((message) => {
    console.log('Received message:', message);
    const { type, ...data } = message;

    switch (type) {
      case 'player_joined':
        setPlayerId(data.player_id);
        break;

      case 'lobby_status':
        if (data.state === 'waiting') {
          setGameState('lobby');
          setPlayers(data.players || []);
        } else if (data.state === 'matched') {
          setGameState('lobby');
          setPlayers(data.players || []);
        }
        break;

      case 'table_started':
        setGameState('game');
        setPlayers(data.players || []);
        setTableOrder(data.players || []);
        // Reset bidding state when table starts
        setCurrentBidder(null);
        setPlayerBids({});
        setCurrentBidValue(0);
        setBiddingWinner(null);
        // Reset card player state
        setCurrentCardPlayer(null);
        break;

      case 'cards_dealt':
        setPlayerHand(data.hand || []);
        setLeftPlayerCardCount(10);
        setRightPlayerCardCount(10);
        break;

      case 'card_play_prompt':
        setPlayerHand(data.hand || []);
        setCurrentTrick(data.current_trick || []);
        setValidCards(data.valid_cards || []);
        setCardPlayPrompt({
          message: data.message,
          validCards: data.valid_cards
        });
        setSelectedCardPlay(data.valid_cards.length > 0 ? data.valid_cards[0] : null);
        // Clear discard state when card play prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        // Clear regular prompt
        setPrompt(null);
        setCurrentCardPlayer(null);
        break;

      case 'bid_prompt':
        setPrompt({
          type: 'bid_prompt',
          message: data.message,
          choices: data.choices || []
        });
        // Clear discard and card play state when bid prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        break;

      case 'game_type_prompt':
        setPrompt({
          type: 'game_type_prompt',
          message: data.message,
          choices: data.game_types || [],
          gameTypeValues: data.game_type_values || []
        });
        // Clear discard and card play state when game type prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        break;

      case 'multiplier_prompt':
        setPrompt({
          type: 'multiplier_prompt',
          message: data.message,
          choices: [...data.multipliers || [], "skip"]
        });
        // Clear discard and card play state when multiplier prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        break;

      case 'initial_choice_prompt':
        setPrompt({
          type: 'initial_choice_prompt',
          message: data.message,
          choices: data.choices || []
        });
        // Clear discard and card play state when initial choice prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        setCurrentBidder(null);
      break;

      case 'discard_prompt':
        setDiscardPrompt({
          message: data.message,
          count: data.count
        });
        setSelectedDiscardCards([]);
        // Clear card play state when discard prompt is received
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        break;

      case 'awaiting_bid':
        setCurrentBidder(data.waiting_for_player_id);
        break;

      case 'awaiting_card':
        setCurrentCardPlayer(data.waiting_for_player_id);
        break;

      case 'bid_broadcast':
        setCurrentBidValue(data.bid_value);
        setPlayerBids(prev => ({
          ...prev,
          [data.bidder.id]: { type: 'bid', value: data.bid_value }
        }));
        // Clear current bidder when a bid is made
        if (data.bidder.id === currentBidder) {
          setCurrentBidder(null);
        }
        break;

      case 'pass_broadcast':
        setPlayerBids(prev => ({
          ...prev,
          [data.passer.id]: { type: 'pass', value: data.bid_value }
        }));
        // Clear current bidder when a pass is made
        if (data.passer.id === currentBidder) {
          setCurrentBidder(null);
        }
        break;

      case 'bidding_winner_notification':
        setBiddingWinner({
          winnerId: data.winner_id,
          bidValue: data.bid_value,
          message: data.message
        });
        // Clear current bidder but keep player bids visible until game starts
        setCurrentBidder(null);
        setCurrentBidValue(0);
        break;

      case 'game_declaration_broadcast':
        setGameDeclaration({
          winnerId: data.winner_id,
          choice: data.choice,
          message: data.message
        });
        break;

      case 'game_type_broadcast':
        setGameType({
          winnerId: data.winner_id,
          gameType: data.game_type,
          message: data.message
        });
        break;

      case 'card_played_broadcast':
        // Update current trick when a card is played
        setCurrentTrick(prev => [...prev, {
          position: data.card_index,
          player: data.player_id,
          card: data.card
        }]);
        // Clear current card player when a card is played
        if (data.player_id === currentCardPlayer) {
          setCurrentCardPlayer(null);
        }
        // Decrement card count for the player who played (if not current player)
        if (data.player_id !== playerId) {
          const playerPosition = tableOrder.findIndex(player => player.id === data.player_id);
          if (playerPosition === 1) {
            setLeftPlayerCardCount(prev => Math.max(0, prev - 1));
          } else if (playerPosition === 2) {
            setRightPlayerCardCount(prev => Math.max(0, prev - 1));
          }
        }
        break;

      case 'trick_won_broadcast':
        // Clear current trick when a trick is won
        setCurrentTrick([]);
        // Update card pile counts based on who won the trick
        if (gameInfoRef.current && gameInfoRef.current.declarer && data.winner_id && data.trick) {
          const trickCardCount = data.trick.length;
          if (data.winner_id === gameInfoRef.current.declarer) {
            // Declarer won the trick
            setDeclarerCardsWon(prev => prev + trickCardCount);
          } else {
            // Opponent won the trick
            setOpponentsCardsWon(prev => prev + trickCardCount);
          }
        }
        break;

      case 'game_start_broadcast':
        // Store game info for display during play
        const newGameInfo = {
          declarer: data.declarer,
          gameType: data.game_type,
          isHandGame: data.is_hand_game,
          selectedMultipliers: data.selected_multipliers,
          message: data.message
        };
        setGameInfo(newGameInfo);
        gameInfoRef.current = newGameInfo;
        setGameState('game');
        // Clear bidding state when game starts
        setCurrentBidder(null);
        setPlayerBids({});
        setCurrentBidValue(0);
        setBiddingWinner(null);
        // Clear card player state
        setCurrentCardPlayer(null);
        // Clear game declaration and type state
        setGameDeclaration(null);
        setGameType(null);
        // Clear discard and card play state
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        // Reset card pile counts for new game
        setDeclarerCardsWon(0);
        setOpponentsCardsWon(0);
        // Keep card counts - they should persist during gameplay
        break;

      case 'game_complete_broadcast':
        // Extract game result information
        const result = data.result;
        const declarer = result.declarer;
        const declarerWon = result.declarer_won;
        const declarerPoints = result.declarer_points;
        const actualGameValue = result.actual_game_value;

        // Create completion message based on who won
        let completionMessage;
        if (declarerWon) {
          completionMessage = `Declarer won with ${declarerPoints} points and scored ${actualGameValue} points!`;
        } else {
          completionMessage = `Declarer lost with only ${declarerPoints} points and lost ${Math.abs(actualGameValue)} points.`;
        }

        // Show completion message for 4 seconds
        setGameCompletionMessage(completionMessage);
        setTimeout(() => {
          setGameCompletionMessage(null);
        }, 4000);

        // Handle game completion
        setPlayerHand([]);
        setCurrentTrick([]);
        setValidCards([]);
        setCurrentBidder(null);
        setPlayerBids({});
        setCurrentBidValue(0);
        setBiddingWinner(null);
        setCurrentCardPlayer(null);
        // Clear game declaration and type state
        setGameDeclaration(null);
        setGameType(null);
        // Clear game info
        setGameInfo(null);
        gameInfoRef.current = null;
        // Clear discard and card play state
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCardPlayPrompt(null);
        setSelectedCardPlay(null);
        // Reset card counts
        setLeftPlayerCardCount(0);
        setRightPlayerCardCount(0);
        // Reset card pile counts
        setDeclarerCardsWon(0);
        setOpponentsCardsWon(0);
        break;

      case 'hand_reorder_broadcast':
        // Reorder all player hands based on the broadcast data
        if (data.hand && Array.isArray(data.hand)) {
          setPlayerHand(data.hand);
        }
        break;

      case 'skat_flipped':
        setSkatCards(data.cards || []);
        setShowSkatCards(true);
        // Hide skat cards after 3 seconds if no hand_with_skat message is received
        setTimeout(() => {
          setShowSkatCards(false);
          setSkatCards([]);
        }, 6000);
        break;

      case 'hand_with_skat':
        setPlayerHand(data.cards || []);
        break;

      case 'game_closed':
        // Reset game state and return to lobby
        setGameState('lobby');
        setPlayerHand([]);
        setLeftPlayerCards([]);
        setRightPlayerCards([]);
        setLeftPlayerCardCount(0);
        setRightPlayerCardCount(0);
        setCurrentTrick([]);
        setValidCards([]);
        setPrompt(null);
        setCurrentBidder(null);
        setPlayerBids({});
        setCurrentBidValue(0);
        setBiddingWinner(null);
        setCurrentCardPlayer(null);
        setGameDeclaration(null);
        setGameType(null);
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setSkatCards([]);
        setShowSkatCards(false);
        setDeclarerCardsWon(0);
        setOpponentsCardsWon(0);
        console.log('Game closed, returning to lobby');
        break;

      case 'hand_after_discard':
        setPlayerHand(data.hand || []);
        break;

      case 'player_disconnected':
        // Show disconnection notice with reconnection deadline
        const playerPosition = tableOrder.findIndex(player => player.id === data.player_id);
        let positionText = 'player';
        if (playerPosition === 1) positionText = 'left player';
        else if (playerPosition === 2) positionText = 'right player';

        setDisconnectedPlayer({
          playerId: data.player_id,
          message: `${positionText} disconnected... waiting ${Math.ceil(data.reconnection_deadline_ms / 1000)} seconds for them to reconnect`,
          deadline: data.reconnection_deadline_ms
        });

        // Clear disconnection notice after the deadline
        setTimeout(() => {
          setDisconnectedPlayer(null);
        }, data.reconnection_deadline_ms);
        break;

      case 'player_reconnected':
        setDisconnectedPlayer(null);

        // Always show reconnection message
        const reconnectedPlayerPosition = tableOrder.findIndex(player => player.id === data.player_id);
        let reconnectedPositionText = 'player';
        if (reconnectedPlayerPosition === 1) reconnectedPositionText = 'left player';
        else if (reconnectedPlayerPosition === 2) reconnectedPositionText = 'right player';

        setReconnectionMessage(`${reconnectedPositionText} reconnected!`);

        // Clear reconnection message after 2 seconds
        setTimeout(() => {
          setReconnectionMessage(null);
        }, 2000);
        break;

      case 'scores_update_broadcast':
        setScoresModal({
          message: data.message,
          playerScores: data.player_scores
        });
        
        // Hide scores modal after 5 seconds
        setTimeout(() => {
          setScoresModal(null);
        }, 5000);
        break;

      default:
        console.log('Unhandled message type:', type, data);
    }
  }, [currentBidder, currentBidValue, currentCardPlayer, disconnectedPlayer, tableOrder, playerId]);

  const { connect, disconnect, sendMessage, isConnected } = useWebSocket(
    `ws://${window.location.hostname}:8080/ws`,
    handleWebSocketMessage
  );

  const handleLogin = useCallback((name) => {
    setPlayerName(name);
    setGameState('lobby');
    connect();
  }, [connect]);

  const handleCardClick = useCallback((cardIndex) => {
    if (validCards.includes(cardIndex)) {
      sendMessage(cardIndex);
      setPrompt(null);
    }
  }, [validCards, sendMessage]);

  const handlePromptChoice = useCallback((choice) => {
    sendMessage(choice);
    setPrompt(null);
  }, [sendMessage]);

  const handlePromptClose = useCallback(() => {
    setPrompt(null);
  }, []);

  const handleDiscardCardClick = useCallback((cardIndex) => {
    if (!discardPrompt) return;

    setSelectedDiscardCards(prev => {
      if (prev.includes(cardIndex)) {
        // Remove card if already selected
        return prev.filter(index => index !== cardIndex);
      } else if (prev.length < discardPrompt.count) {
        // Add card if under the limit
        return [...prev, cardIndex];
      }
      // Don't add if at limit
      return prev;
    });
  }, [discardPrompt]);

  const handleDiscardSubmit = useCallback(() => {
    if (selectedDiscardCards.length === discardPrompt.count) {
      sendMessage(selectedDiscardCards);
      setDiscardPrompt(null);
      setSelectedDiscardCards([]);
    }
  }, [selectedDiscardCards, discardPrompt, sendMessage]);

  const handleCardPlayClick = useCallback((cardIndex) => {
    if (!cardPlayPrompt || !cardPlayPrompt.validCards.includes(cardIndex)) return;
    setSelectedCardPlay(cardIndex);
  }, [cardPlayPrompt]);

  const handleCardPlaySubmit = useCallback(() => {
    if (selectedCardPlay !== null && cardPlayPrompt && cardPlayPrompt.validCards.includes(selectedCardPlay)) {
      sendMessage(selectedCardPlay);
      setPlayerHand(prev => prev.filter((_, index) => index !== selectedCardPlay));
      setCardPlayPrompt(null);
      setSelectedCardPlay(null);
    }
  }, [selectedCardPlay, cardPlayPrompt, sendMessage]);

  const handleKeyDown = useCallback((event) => {
    if (!cardPlayPrompt || !cardPlayPrompt.validCards.length) return;

    if (event.key === 'ArrowLeft' || event.key === 'ArrowRight') {
      event.preventDefault();
      const currentIndex = cardPlayPrompt.validCards.indexOf(selectedCardPlay);
      let newIndex;

      if (event.key === 'ArrowLeft') {
        newIndex = currentIndex > 0 ? currentIndex - 1 : cardPlayPrompt.validCards.length - 1;
      } else {
        newIndex = currentIndex < cardPlayPrompt.validCards.length - 1 ? currentIndex + 1 : 0;
      }

      setSelectedCardPlay(cardPlayPrompt.validCards[newIndex]);
    } else if (event.key === 'Enter') {
      event.preventDefault();
      handleCardPlaySubmit();
    }
  }, [cardPlayPrompt, selectedCardPlay, handleCardPlaySubmit]);

  useEffect(() => {
    if (cardPlayPrompt) {
      document.addEventListener('keydown', handleKeyDown);
      return () => document.removeEventListener('keydown', handleKeyDown);
    }
  }, [cardPlayPrompt, handleKeyDown]);

  useEffect(() => {
    return () => {
      disconnect();
    };
  }, [disconnect]);

  const renderContent = () => {
    switch (gameState) {
      case 'login':
        return <LoginScreen onLogin={handleLogin} />;

      case 'lobby':
        return <LobbyScreen players={players} />;

      case 'game':
        return (
          <GameBoard
            playerHand={playerHand}
            leftPlayerCards={leftPlayerCards}
            rightPlayerCards={rightPlayerCards}
            leftPlayerCardCount={leftPlayerCardCount}
            rightPlayerCardCount={rightPlayerCardCount}
            currentTrick={currentTrick}
            onCardClick={handleCardClick}
            validCards={validCards}
            playerId={playerId}
            tableOrder={tableOrder}
            currentBidder={currentBidder}
            playerBids={playerBids}
            currentBidValue={currentBidValue}
            biddingWinner={biddingWinner}
            currentCardPlayer={currentCardPlayer}
            gameDeclaration={gameDeclaration}
            gameType={gameType}
            discardPrompt={discardPrompt}
            selectedDiscardCards={selectedDiscardCards}
            onDiscardCardClick={handleDiscardCardClick}
            onDiscardSubmit={handleDiscardSubmit}
            cardPlayPrompt={cardPlayPrompt}
            selectedCardPlay={selectedCardPlay}
            onCardPlayClick={handleCardPlayClick}
            onCardPlaySubmit={handleCardPlaySubmit}
            skatCards={skatCards}
            showSkatCards={showSkatCards}
            hasActiveBidPrompt={prompt && prompt.type === 'bid_prompt'}
            hasInitialChoicePrompt={prompt && prompt.type === 'initial_choice_prompt'}
            gameInfo={gameInfo}
            declarerCardsWon={declarerCardsWon}
            opponentsCardsWon={opponentsCardsWon}
          />
        );

      default:
        return <div>Unknown game state: {gameState}</div>;
    }
  };

  return (
    <div className="App">
      {scoresModal ? (
        <div className="scores-modal-overlay">
          <div className="scores-modal">
            <h2>Scoreboard</h2>
            <div className="scores-list">
              {scoresModal.playerScores.map((playerScore) => {
                const player = players.find(p => p.id === playerScore.player_id);
                const playerName = player ? player.name : 'Unknown Player';
                return (
                  <div key={playerScore.player_id} className="score-item">
                    <span className="player-name">{playerName}</span>
                    <span className="player-score">{playerScore.score}</span>
                  </div>
                );
              })}
            </div>
          </div>
        </div>
      ) : (
        renderContent()
      )}

      {prompt && prompt.type !== 'card_play_prompt' && (
        <PromptModal
          isOpen={!!prompt}
          message={prompt.message}
          choices={prompt.choices}
          type={prompt.type}
          gameTypeValues={prompt.gameTypeValues}
          onChoice={handlePromptChoice}
          onClose={handlePromptClose}
        />
      )}

      {!isConnected && gameState !== 'login' && (
        <div className="connection-status">
          <p>Connecting to server...</p>
        </div>
      )}

      {disconnectedPlayer && (
        <div className="disconnection-notice">
          <p>{disconnectedPlayer.message}</p>
        </div>
      )}

    {reconnectionMessage && (
        <div className="reconnection-message">
          <p>{reconnectionMessage}</p>
        </div>
      )}

      {gameCompletionMessage && (
        <div className="game-completion-message">
          <p>{gameCompletionMessage}</p>
        </div>
      )}

    </div>
  );
};

export default App;
