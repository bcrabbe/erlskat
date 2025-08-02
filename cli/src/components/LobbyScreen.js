const React = require('react');
const { Box, Text } = require('ink');

/**
 * Lobby screen component - displays waiting state and player status
 */
const LobbyScreen = ({
  connectionState,
  lobbyState,
  players = [],
  reconnectInfo = null
}) => {

  const renderConnectionStatus = () => {
    switch (connectionState) {
      case 'connecting':
        return React.createElement(Text, { color: "yellow" }, "Connecting to server...");
      case 'connected':
        return React.createElement(Text, { color: "green" }, "Connected to server");
      case 'disconnected':
        return React.createElement(Text, { color: "red" }, "Disconnected from server");
      default:
        return React.createElement(Text, { color: "gray" }, "Unknown connection state");
    }
  };

  const renderLobbyStatus = () => {
    if (!lobbyState) {
      return React.createElement(Text, null, "Waiting to join lobby...");
    }

    switch (lobbyState) {
      case 'waiting':
        return React.createElement(Box, { flexDirection: "column" },
          React.createElement(Text, { color: "yellow" }, "Waiting for players..."),
          React.createElement(Text, null, `Players in lobby: ${players.length}/3`),
          players.length > 0 && React.createElement(Box, { flexDirection: "column" },
            React.createElement(Text, null, "Current players:"),
            ...players.map((player, index) =>
              React.createElement(Text, { key: index }, `  • ${player}`)
            )
          )
        );
      case 'matched':
        return React.createElement(Box, { flexDirection: "column" },
          React.createElement(Text, { color: "green" }, "All players found! Starting game..."),
          React.createElement(Text, null, "Players:"),
          ...players.map((player, index) =>
            React.createElement(Text, { key: index }, `  • ${player}`)
          )
        );
      default:
        return React.createElement(Text, null, `Unknown lobby state: ${lobbyState}`);
    }
  };

  const renderReconnectInfo = () => {
    if (!reconnectInfo) return null;

    const elements = [
      React.createElement(Text, { color: "orange", bold: true }, "Reconnection Status")
    ];

    if (reconnectInfo.disconnectedPlayer) {
      elements.push(
        React.createElement(Text, null, `Player ${reconnectInfo.disconnectedPlayer} disconnected`)
      );
    }

    if (reconnectInfo.timedOutPlayer) {
      elements.push(
        React.createElement(Text, null, `Player ${reconnectInfo.timedOutPlayer} timed out`)
      );
    }

    if (reconnectInfo.reconnectDeadline) {
      elements.push(
        React.createElement(Text, null, `Reconnection deadline: ${new Date(reconnectInfo.reconnectDeadline).toLocaleTimeString()}`)
      );
    }

    return React.createElement(Box, {
      flexDirection: "column",
      borderStyle: "single",
      borderColor: "orange",
      padding: 1
    }, ...elements);
  };

  const elements = [
    // Header
    React.createElement(Box, { flexDirection: "column", marginBottom: 2 },
      React.createElement(Text, { bold: true, color: "blue" }, "♠♥♦♣ Erlskat - Multiplayer Skat ♣♦♥♠"),
      React.createElement(Text, { color: "gray" }, "WebSocket-based 3-player Skat card game")
    ),

    // Connection status
    React.createElement(Box, { flexDirection: "column", marginBottom: 2, borderStyle: "single", padding: 1 },
      React.createElement(Text, { bold: true }, "Connection Status"),
      renderConnectionStatus()
    ),

    // Lobby status
    React.createElement(Box, { flexDirection: "column", marginBottom: 2, borderStyle: "single", padding: 1 },
      React.createElement(Text, { bold: true }, "Lobby Status"),
      renderLobbyStatus()
    ),

    // Reconnection info
    renderReconnectInfo()
  ];

  // Instructions
  if (connectionState === 'connected') {
    elements.push(
      React.createElement(Box, { flexDirection: "column", marginTop: 2 },
        React.createElement(Text, { color: "gray", dimColor: true }, "Once 3 players join, the game will start automatically."),
        React.createElement(Text, { color: "gray", dimColor: true }, "Press Ctrl+C to exit.")
      )
    );
  }

  if (connectionState === 'disconnected') {
    elements.push(
      React.createElement(Box, { flexDirection: "column", marginTop: 2 },
        React.createElement(Text, { color: "red" }, "Connection lost. Attempting to reconnect...")
      )
    );
  }

  return React.createElement(Box, { flexDirection: "column", height: "100%", padding: 2 }, ...elements.filter(Boolean));
};

module.exports = LobbyScreen;
