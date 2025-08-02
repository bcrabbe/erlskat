const React = require('react');
const { Box, Text } = require('ink');

/**
 * Lobby screen component - displays waiting state and player status
 */
const LobbyScreen = ({
  connectionState,
  lobbyState,
  players = []
}) => {

  const renderConnectionStatus = () => {
    switch (connectionState) {
      case 'connecting':
        return <Text color="yellow">Connecting to server...</Text>;
      case 'connected':
        return <Text color="green">Connected to server</Text>;
      case 'disconnected':
        return <Text color="red">Disconnected from server</Text>;
      default:
        return <Text color="gray">Unknown connection state</Text>;
    }
  };

  const renderLobbyStatus = () => {
    if (!lobbyState) {
      return <Text>Waiting to join lobby...</Text>;
    }

    switch (lobbyState) {
      case 'waiting':
        return (
          <Box flexDirection="column">
            <Text color="yellow">Waiting for players...</Text>
            <Text>Players in lobby: {players.length}/3</Text>
            {players.length > 0 && (
              <Box flexDirection="column">
                <Text>Current players:</Text>
                {players.map((player, index) => (
                  <Text key={index}>  • {player}</Text>
                ))}
              </Box>
            )}
          </Box>
        );
      case 'matched':
        return (
          <Box flexDirection="column">
            <Text color="green">All players found! Starting game...</Text>
            <Text>Players:</Text>
            {players.map((player, index) => (
              <Text key={index}>  • {player}</Text>
            ))}
          </Box>
        );
      default:
        return <Text>Unknown lobby state: {lobbyState}</Text>;
    }
  };


  const elements = [
    // Header
    <Box flexDirection="column" marginBottom={2} key="header">
      <Text bold color="blue">♠♥♦♣ Erlskat - Multiplayer Skat ♣♦♥♠</Text>
      <Text color="gray">WebSocket-based 3-player Skat card game</Text>
    </Box>,

    // Connection status
    <Box flexDirection="column" marginBottom={2} borderStyle="single" padding={1} key="connection">
      <Text bold>Connection Status</Text>
      {renderConnectionStatus()}
    </Box>,

    // Lobby status
    <Box flexDirection="column" marginBottom={2} borderStyle="single" padding={1} key="lobby">
      <Text bold>Lobby Status</Text>
      {renderLobbyStatus()}
    </Box>,

  ].filter(Boolean);

  // Instructions
  if (connectionState === 'connected') {
    elements.push(
      <Box flexDirection="column" marginTop={2} key="instructions">
        <Text color="gray" dimColor>Once 3 players join, the game will start automatically.</Text>
        <Text color="gray" dimColor>Press Ctrl+C to exit.</Text>
      </Box>
    );
  }

  if (connectionState === 'disconnected') {
    elements.push(
      <Box flexDirection="column" marginTop={2} key="reconnecting">
        <Text color="red">Connection lost. Attempting to reconnect...</Text>
      </Box>
    );
  }

  return (
    <Box flexDirection="column" height="100%" padding={2}>
      {elements}
    </Box>
  );
};

module.exports = LobbyScreen;
