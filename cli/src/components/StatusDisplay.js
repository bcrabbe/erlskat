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
    const latestMessages = messages.slice(-maxMessages);
    // Add unique IDs to messages for stable keys
    const messagesWithIds = latestMessages.map((msg, index) => ({
      ...msg,
      _displayId: `${messages.length - maxMessages + index}-${JSON.stringify(msg).slice(0, 50)}`
    }));
    setDisplayMessages(messagesWithIds);
  }, [messages, maxMessages]);

  const formatMessage = (message) => {
    if (!message || typeof message !== 'object') {
      return String(message);
    }

    const {type, ...rest} = message;

    // Handle messages with explicit message field (prefer this over custom formatting)
    if (type && rest.message) {
      return `[${type}] ${rest.message}`;
    }

    // Handle specific message types with custom formatting
    if (type) {
      switch (type) {
        case 'bid_broadcast':
          return `[${type}] Player ${rest.player_id?.slice(0, 8)}... bids ${rest.bid}`;
        case 'pass_broadcast':
          return `[${type}] Player ${rest.player_id?.slice(0, 8)}... passes at ${rest.bid}`;
        case 'card_played_broadcast':
          return `[${type}] Player ${rest.player_id?.slice(0, 8)}... played ${rest.card || 'a card'}`;
        case 'trick_won_broadcast':
          return `[${type}] Player ${rest.winner_id?.slice(0, 8)}... won the trick`;
        case 'game_start_broadcast':
          return `[${type}] Game started - ${rest.game_type} by ${rest.declarer?.slice(0, 8)}...`;
        case 'game_complete_broadcast':
          return `[${type}] Game complete - ${rest.result}`;
        case 'player_disconnected':
          return `[${type}] Player ${rest.player_id?.slice(0, 8)}... disconnected`;
        case 'player_timed_out':
          return `[${type}] Player ${rest.player_id?.slice(0, 8)}... timed out`;
        case 'table_started':
          return `[${type}] Table started with ${rest.players?.length || 0} players`;
        case 'awaiting_bid':
          return `[${type}] Waiting for ${rest.waiting_for_player_id?.slice(0, 8)}... to bid ${rest.bid_value}`;
        case 'bidding_winner_notification':
          return `[${type}] ${rest.winner_id?.slice(0, 8)}... won bidding with ${rest.bid_value}`;
        case 'bidding_complete':
          return `[${type}] Bidding phase complete`;
        case 'hand_reorder_broadcast':
          return `[${type}] Hands reordered for ${rest.game_type} game`;
        case 'skat_flipped':
          return `[${type}] Skat cards revealed`;
        case 'scores_update_broadcast':
          return `[${type}] Player scores updated`;
        case 'next_hand_starting_broadcast':
          return `[${type}] Hand ${rest.hand_number} starting`;
        case 'game_closed':
          return `[${type}] Game closed - returning to lobby`;
        case 'lobby_status':
          return `[${type}] Lobby ${rest.state} - ${rest.players?.length || 0} players`;
        case 'player_joined':
          return `[${type}] Player ${rest.player_id?.slice(0, 8)}... joined`;
        default:
          // For other types, try to extract meaningful info without JSON.stringify
          const entries = Object.entries(rest);
          if (entries.length === 0) {
            return `[${type}]`;
          } else {
            return `[${type}] ${entries.join(', ')}`;
          }
      }
    }
p
    return String(message);
  };

  const getMessageColor = (message) => {

    switch (message.type) {
      case 'bid_broadcast':
      case 'pass_broadcast':
        return 'cyan';
      case 'game_start_broadcast':
      case 'game_complete_broadcast':
      case 'table_started':
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
      case 'awaiting_bid':
      case 'bidding_winner_notification':
      case 'bidding_complete':
        return 'cyan';
      case 'hand_reorder_broadcast':
      case 'skat_flipped':
        return 'magenta';
      case 'scores_update_broadcast':
      case 'next_hand_starting_broadcast':
        return 'green';
      case 'game_closed':
      case 'lobby_status':
      case 'player_joined':
        return 'blue';
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
    <Box flexDirection="column" borderStyle="single" borderColor="white" padding={1} width="100%" height="100%">
      {/* Header with game status */}
      <Box flexDirection="row" justifyContent="space-between" flexShrink={0}>
        <Box flexDirection="row">
          <Text bold color="blue">Erlskat </Text>
          {renderGameInfo()}
        </Box>
        <Box flexDirection="row">
          <Text color="gray">Connection: </Text>
          {renderConnectionStatus()}
        </Box>
      </Box>

      {/* Message history */}
      <Box flexDirection="column" flexGrow={10} minHeight={0}>
        <Text bold color="gray">Recent Activity:</Text>
        <Box flexDirection="column" flexGrow={1}>
          {displayMessages.length === 0 ? (
            <Text color="gray" dimColor>No messages yet...</Text>
          ) : (
            displayMessages.map((message, index) => {
              const formattedMessage = formatMessage(message);
              // Pad message to ensure full line clear
              const paddedMessage = formattedMessage.padEnd(120, ' ');
              return (
                <Text
                  key={message._displayId || `msg-${index}`}
                  color={getMessageColor(message)}
                >
                  {paddedMessage}
                </Text>
              );
            })
          )}
        </Box>
      </Box>

      {/* Scroll indicator if there are more messages */}
      {displayMessages.length >= maxMessages && (
        <Box flexShrink={0}>
          <Text color="gray" dimColor>
            ... (showing last {maxMessages} messages)
          </Text>
        </Box>
      )}
    </Box>
  );
};

module.exports = StatusDisplay;
