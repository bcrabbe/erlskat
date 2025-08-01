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

  const handleWebSocketMessage = useCallback((message) => {
    console.log('Received message:', message);

    const { type, ...data } = message;

    switch (type) {
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
        break;

      case 'bid_prompt':
        setPrompt({
          type: 'bid_prompt',
          message: data.message,
          choices: data.choices || []
        });
        break;

      case 'game_type_prompt':
        setPrompt({
          type: 'game_type_prompt',
          message: data.message,
          choices: data.game_types || []
        });
        break;

      case 'multiplier_prompt':
        setPrompt({
          type: 'multiplier_prompt',
          message: data.message,
          choices: data.multipliers || []
        });
        break;

      case 'initial_choice_prompt':
        setPrompt({
          type: 'initial_choice_prompt',
          message: data.message,
          choices: data.choices || []
        });
        break;

      case 'discard_prompt':
        setPrompt({
          type: 'discard_prompt',
          message: data.message,
          choices: Array.from({ length: data.count }, (_, i) => i)
        });
        break;

      case 'card_played_broadcast':
        // Update current trick when a card is played
        setCurrentTrick(prev => [...prev, {
          position: data.card_index,
          player: data.player_id,
          card: data.card
        }]);
        break;

      case 'trick_won_broadcast':
        // Clear current trick when a trick is won
        setCurrentTrick([]);
        break;

      case 'game_start_broadcast':
        setGameState('game');
        break;

      case 'game_complete_broadcast':
        // Handle game completion
        setGameState('lobby');
        setPlayerHand([]);
        setCurrentTrick([]);
        setValidCards([]);
        break;

      default:
        console.log('Unhandled message type:', type, data);
    }
  }, []);

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
          onChoice={handlePromptChoice}
          onClose={handlePromptClose}
        />
      )}

      {!isConnected && gameState !== 'login' && (
        <div className="connection-status">
          <p>Connecting to server...</p>
        </div>
      )}
    </div>
  );
};

export default App;
