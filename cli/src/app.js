const importJsx = require('import-jsx');
const React = require('react');
const { useState, useEffect } = require('react');
const { Box, Text } = require('ink');
const WebSocketManager = require('./websocket');
const MessageHandler = require('./utils/messageHandler');
const FallbackInputHandler = require('./utils/fallbackInput');
const LobbyScreen = importJsx('./components/LobbyScreen');
const GameScreen = importJsx('./components/GameScreen');
const PromptHandler = importJsx('./components/PromptHandler');
const StatusDisplay = importJsx('./components/StatusDisplay');

/**
 * Main application component
 * Manages WebSocket connection, message handling, and UI state
 */
const App = ({ wsUrl, debug = false }) => {
  const [wsManager] = useState(() => new WebSocketManager(wsUrl, debug));
  const [messageHandler] = useState(() => new MessageHandler(debug));
  const [fallbackInput] = useState(() => new FallbackInputHandler());
  const [connectionState, setConnectionState] = useState('disconnected');
  const [gameState, setGameState] = useState({
    phase: 'lobby',
    playerHand: null,
    currentPrompt: null,
    gameInfo: {},
    connectionInfo: {}
  });
  const [messages, setMessages] = useState([]);
  const [currentNonCardPrompt, setCurrentNonCardPrompt] = useState(null);
  const [error, setError] = useState(null);
  const [isRawModeSupported] = useState(() => process.stdin.isTTY && process.stdin.setRawMode);

  // Initialize WebSocket connection and message handlers
  useEffect(() => {
    let isMounted = true;

    // Set up message handler event listeners with mounted check
    messageHandler.setStateUpdateHandler((newState) => {
      if (!isMounted) return;
      setGameState(newState);

      // Handle non-card prompts separately
      if (newState.currentPrompt) {
        const promptType = newState.currentPrompt.type;
        if (!['card_play_prompt', 'discard_prompt'].includes(promptType)) {
          setCurrentNonCardPrompt(newState.currentPrompt);
        } else {
          setCurrentNonCardPrompt(null);
        }
      } else {
        setCurrentNonCardPrompt(null);
      }
    });

    messageHandler.setBroadcastHandler((message) => {
      if (!isMounted) return;
      setMessages(prev => [...prev, message]);
    });

    messageHandler.setErrorHandler((error) => {
      if (!isMounted) return;
      setError(error.message);
      setMessages(prev => [...prev, error]);
    });

    messageHandler.setConnectionChangeHandler((message) => {
      if (!isMounted) return;
      if (message.type === 'game_closed') {
        // Reset to lobby state
        setCurrentNonCardPrompt(null);
        setMessages([message]); // Start fresh with just this message
        setError(null);
      } else {
        setMessages(prev => [...prev, message]);
      }
    });

    // Set up WebSocket event listeners
    wsManager.setMessageHandler((message) => {
      messageHandler.handleMessage(message);
    });

    wsManager.setConnectionChangeHandler((state) => {
      if (!isMounted) return;
      setConnectionState(state);
    });

    wsManager.setErrorHandler((error) => {
      if (!isMounted) return;
      setError(error.message);
    });

    // Connect to WebSocket
    wsManager.connect();

    // Initialize fallback input if raw mode is not supported
    if (!isRawModeSupported) {
      fallbackInput.initialize((command, data) => {
        handleFallbackInput(command, data);
      });
    }

    // Cleanup on unmount
    return () => {
      isMounted = false;
      wsManager.disconnect();
      fallbackInput.close();
      // Clear all handlers to prevent memory leaks
      messageHandler.setStateUpdateHandler(null);
      messageHandler.setBroadcastHandler(null);
      messageHandler.setErrorHandler(null);
      messageHandler.setConnectionChangeHandler(null);
      wsManager.setMessageHandler(null);
      wsManager.setConnectionChangeHandler(null);
      wsManager.setErrorHandler(null);
    };
  }, [wsManager, messageHandler, fallbackInput, isRawModeSupported]);

  // Handle fallback input when raw mode is not supported
  const handleFallbackInput = (command, data) => {
    try {
      switch (command) {
        case 'hold':
        case 'pass':
        case 'hand':
        case 'skat':
        case 'diamonds':
        case 'hearts':
        case 'spades':
        case 'clubs':
        case 'grand':
        case 'null':
          handlePromptResponse(command);
          break;
        case 'card_select':
          handleCardSelection([data]);
          break;
        case 'multi_card_select':
          handleCardSelection(data);
          break;
        default:
          console.log(`Unknown command: ${command}`);
      }
    } catch (error) {
      setError(`Failed to handle input: ${error.message}`);
    }
  };

  // Handle card selection (for card_play_prompt and discard_prompt)
  const handleCardSelection = (cardIndices) => {
    try {
      if (gameState.currentPrompt?.type === 'card_play_prompt') {
        // Single card selection
        const cardIndex = cardIndices[0];
        wsManager.sendMessage({ card_index: cardIndex });
      } else if (gameState.currentPrompt?.type === 'discard_prompt') {
        // Multi-card selection
        wsManager.sendMessage({ card_indices: cardIndices });
      }
    } catch (error) {
      setError(`Failed to send card selection: ${error.message}`);
    }
  };

  // Handle non-card prompt responses
  const handlePromptResponse = (response) => {
    try {
      if (currentNonCardPrompt?.type === 'bid_prompt') {
        wsManager.sendMessage(response);
      } else if (currentNonCardPrompt?.type === 'initial_choice_prompt') {
        wsManager.sendMessage(response);
      } else if (currentNonCardPrompt?.type === 'game_type_prompt') {
        wsManager.sendMessage(response);
      } else if (currentNonCardPrompt?.type === 'multiplier_prompt') {
        wsManager.sendMessage(response);
      }

      // Clear the prompt after response
      setCurrentNonCardPrompt(null);
    } catch (error) {
      setError(`Failed to send response: ${error.message}`);
    }
  };

  // Clear error after some time
  useEffect(() => {
    if (error) {
      const timer = setTimeout(() => setError(null), 5000);
      return () => clearTimeout(timer);
    }
  }, [error]);

  // Update fallback input prompt when game state changes
  useEffect(() => {
    if (!isRawModeSupported && currentNonCardPrompt) {
      const promptText = currentNonCardPrompt.message || 'Enter command:';
      fallbackInput.setPrompt(promptText);
    }
  }, [currentNonCardPrompt, isRawModeSupported, fallbackInput]);

  const renderMainContent = () => {
    // Show lobby when in lobby phase or disconnected
    if (gameState.phase === 'lobby' || connectionState === 'disconnected') {
      return (
        <LobbyScreen
          connectionState={connectionState}
          lobbyState={gameState.connectionInfo?.lobbyState}
          players={gameState.connectionInfo?.players || []}
        />
      );
    }

    // Show game screen for active gameplay
    return (
      <Box flexDirection="column" height="100%">
        {/* Status and messages area */}
        <Box flexGrow={1}>
          <StatusDisplay
            messages={messages} // Show recent messages
            gameState={gameState}
            connectionState={connectionState}
          />
        </Box>

        {/* Non-card prompts overlay */}
        {currentNonCardPrompt && (
          <PromptHandler
            prompt={currentNonCardPrompt}
            onResponse={handlePromptResponse}
            debug={debug}
          />
        )}

        {/* Game screen with hand display */}
        <GameScreen
          gameState={gameState}
          onCardSelection={handleCardSelection}
          onChoiceSelection={handlePromptResponse}
          debug={debug}
        />
      </Box>
    );
  };

  return (
    <Box flexDirection="column" height="100%">
      {/* Error display */}
      {error && (
        <Box borderStyle="single" borderColor="red" padding={1}>
          <Text color="red" bold>Error: {error}</Text>
        </Box>
      )}

      {/* Main content */}
      {renderMainContent()}

      {/* Debug info */}
      {debug && (
        <Box borderStyle="single" borderColor="gray" padding={1}>
          <Text color="gray">
            Debug: Connection={connectionState}, Phase={gameState.phase},
            Hand={gameState.playerHand?.length || 0} cards,
            Prompt={gameState.currentPrompt?.type || 'none'}
          </Text>
        </Box>
      )}
    </Box>
  );
};

module.exports = App;
