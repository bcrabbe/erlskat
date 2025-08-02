const React = require('react');
const { useState, useEffect } = require('react');
const { Box, Text } = require('ink');

/**
 * Status display component for showing game messages and broadcasts
 */
const StatusDisplay = ({ 
  messages = [],
  gameState,
  connectionState,
  maxMessages = 15
}) => {
  const [displayMessages, setDisplayMessages] = useState([]);

  // Update displayed messages when new messages arrive
  useEffect(() => {
    if (messages.length > 0) {
      setDisplayMessages(prev => {
        const newMessages = [...prev, ...messages].slice(-maxMessages);
        return newMessages;
      });
    }
  }, [messages, maxMessages]);

  const formatMessage = (message) => {
    if (typeof message === 'string') {
      return message;
    }

    if (message.type && message.message) {
      return `[${message.type}] ${message.message}`;
    }

    return JSON.stringify(message);
  };

  const getMessageColor = (message) => {
    if (typeof message === 'string') {
      return 'white';
    }

    switch (message.type) {
      case 'bid_broadcast':
      case 'pass_broadcast':
        return 'cyan';
      case 'game_start_broadcast':
      case 'game_complete_broadcast':
        return 'green';
      case 'card_played_broadcast':
      case 'trick_won_broadcast':
        return 'blue';
      case 'player_disconnected':
      case 'player_timed_out':
        return 'red';
      case 'error':
      case 'invalid_card_error':
      case 'card_play_error':
        return 'red';
      case 'game_declaration_broadcast':
      case 'game_type_broadcast':
        return 'yellow';
      default:
        return 'white';
    }
  };

  const renderGameInfo = () => {
    if (gameState.phase === 'lobby') {
      return (
        <Text color="blue">
          Waiting in lobby... Players: {gameState.connectionInfo?.players?.length || 0}/3
        </Text>
      );
    }

    if (gameState.gameInfo?.declarer) {
      return (
        <Text color="green">
          Game: {gameState.gameInfo.gameType} | 
          Declarer: {gameState.gameInfo.declarer}
          {gameState.gameInfo.isHandGame && ' (Hand)'}
          {gameState.gameInfo.selectedMultipliers?.length > 0 && 
            ` + ${gameState.gameInfo.selectedMultipliers.join(', ')}`}
        </Text>
      );
    }

    return (
      <Text color="yellow">
        Phase: {gameState.phase}
      </Text>
    );
  };

  const renderConnectionStatus = () => {
    let statusColor = 'green';
    let statusText = 'Connected';

    switch (connectionState) {
      case 'connecting':
        statusColor = 'yellow';
        statusText = 'Connecting...';
        break;
      case 'disconnected':
        statusColor = 'red';
        statusText = 'Disconnected';
        break;
    }

    return (
      <Text color={statusColor}>
        {statusText}
      </Text>
    );
  };

  return (
    <Box flexDirection="column" borderStyle="single" borderColor="white" padding={1}>
      {/* Header with game status */}
      <Box flexDirection="row" justifyContent="space-between" marginBottom={1}>
        <Box flexDirection="column">
          <Text bold color="blue">Erlskat Game Status</Text>
          {renderGameInfo()}
        </Box>
        <Box flexDirection="column" alignItems="flex-end">
          <Text color="gray">Connection:</Text>
          {renderConnectionStatus()}
        </Box>
      </Box>

      {/* Message history */}
      <Box flexDirection="column" flexGrow={1}>
        <Text bold color="gray" marginBottom={1}>Recent Activity:</Text>
        {displayMessages.length === 0 ? (
          <Text color="gray" dimColor>No messages yet...</Text>
        ) : (
          displayMessages.map((message, index) => (
            <Text key={index} color={getMessageColor(message)} wrap="truncate">
              {formatMessage(message)}
            </Text>
          ))
        )}
      </Box>

      {/* Scroll indicator if there are more messages */}
      {displayMessages.length >= maxMessages && (
        <Text color="gray" dimColor>
          ... (showing last {maxMessages} messages)
        </Text>
      )}
    </Box>
  );
};

module.exports = StatusDisplay;