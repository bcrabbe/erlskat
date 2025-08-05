import React, { useState, useEffect, useCallback } from 'react';
import { useWebSocket } from './utils/websocket';
import LoginScreen from './components/LoginScreen';
import LobbyScreen from './components/LobbyScreen';
import GameBoard from './components/GameBoard';
import PromptModal from './components/PromptModal';
import './App.css';

const App = () => {
  const [gameState, setGameState] = useState('login'); // login, lobby, game
  const [playerName, setPlayerName] = useState('');
  const [players, setPlayers] = useState([]);
  const [playerHand, setPlayerHand] = useState([]);
  const [leftPlayerCards, setLeftPlayerCards] = useState([]);
  const [rightPlayerCards, setRightPlayerCards] = useState([]);
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

  // New state for skat cards display
  const [skatCards, setSkatCards] = useState([]);
  const [showSkatCards, setShowSkatCards] = useState(false);

  // New state for player disconnection
  const [disconnectedPlayer, setDisconnectedPlayer] = useState(null);

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
        break;

      case 'card_play_prompt':
        setPlayerHand(data.hand || []);
        setCurrentTrick(data.current_trick || []);
        setValidCards(data.valid_cards || []);
        setPrompt({
          type: 'card_play_prompt',
          message: data.message,
          choices: data.valid_cards.map(index => index)
        });
        // Clear discard state when card play prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        break;

      case 'bid_prompt':
        setPrompt({
          type: 'bid_prompt',
          message: data.message,
          choices: data.choices || []
        });
        // Clear discard state when bid prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        break;

      case 'game_type_prompt':
        setPrompt({
          type: 'game_type_prompt',
          message: data.message,
          choices: data.game_types || [],
          gameTypeValues: data.game_type_values || []
        });
        // Clear discard state when game type prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        break;

      case 'multiplier_prompt':
        setPrompt({
          type: 'multiplier_prompt',
          message: data.message,
          choices: [...data.multipliers || [], "skip"]
        });
        // Clear discard state when multiplier prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        break;

      case 'initial_choice_prompt':
        setPrompt({
          type: 'initial_choice_prompt',
          message: data.message,
          choices: data.choices || []
        });
        // Clear discard state when initial choice prompt is received
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        setCurrentBidder(null);
      break;

      case 'discard_prompt':
        setDiscardPrompt({
          message: data.message,
          count: data.count
        });
        setSelectedDiscardCards([]);
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
        break;

      case 'trick_won_broadcast':
        // Clear current trick when a trick is won
        setCurrentTrick([]);
        break;

      case 'game_start_broadcast':
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
        // Clear discard state
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
        break;

      case 'game_complete_broadcast':
        // Handle game completion
        setGameState('lobby');
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
        // Clear discard state
        setDiscardPrompt(null);
        setSelectedDiscardCards([]);
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

      default:
        console.log('Unhandled message type:', type, data);
    }
  }, [currentBidder, currentBidValue, currentCardPlayer]);

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
            skatCards={skatCards}
            showSkatCards={showSkatCards}
            hasActiveBidPrompt={prompt && prompt.type === 'bid_prompt'}
            hasInitialChoicePrompt={prompt && prompt.type === 'initial_choice_prompt'}
          />
        );

      default:
        return <div>Unknown game state: {gameState}</div>;
    }
  };

  return (
    <div className="App">
      {renderContent()}

      {prompt && (
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
    </div>
  );
};

export default App;
