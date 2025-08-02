# Erlskat CLI Client

A terminal-based client for playing Skat through the erlskat websocket server.

## Features

- **Interactive Terminal UI**: Built with ink.js for a rich terminal experience
- **Visual Card Selection**: Navigate your hand with arrow keys, cards rise when selected
- **Session Persistence**: Maintains connection state between disconnects  
- **Multi-card Selection**: For discarding, select multiple cards with visual feedback
- **Real-time Updates**: Live game state, player actions, and scoring
- **Connection Management**: Automatic reconnection and session restoration

## Installation

```bash
cd cli
npm install
```

## Usage

### Basic Usage
```bash
# Connect to local server (default)
npm start

# Or use the executable directly
./index.js

# Connect to specific host/port
./index.js --host example.com --port 9000

# Enable debug logging
./index.js --debug
```

### Environment Variables
```bash
export ERLSKAT_HOST=your-server.com
export ERLSKAT_PORT=8080
./index.js
```

### Global Installation
```bash
npm install -g
erlskat --host example.com --port 9000
```

## Controls

### Lobby
- Wait for 3 players to join automatically
- Ctrl+C to exit

### Bidding Phase
- **Bid Prompts**: Press `H` to hold, `P` to pass
- **Game Type**: Press numbers 1-4 to select game type
- **Multipliers**: Press numbers to select multipliers
- **Hand vs Skat**: Press `H` for hand game, `S` to see skat

### Card Selection

#### Single Card (Playing Cards)
- **Left/Right Arrows**: Navigate through your hand
- **Enter**: Play selected card
- Selected card rises above the others

#### Multiple Cards (Discarding)
- **Left/Right Arrows**: Navigate through your hand  
- **Enter**: Select/deselect card (up to 2 cards)
- **Shift+Enter**: Submit selected cards for discard
- Selected cards show visual indicators

### Game Play
- Cards are displayed with suit symbols: ♠♥♦♣
- Current trick and game status shown in real-time
- Player actions broadcast to all participants

## Game Flow

1. **Connection**: Auto-connects and joins lobby
2. **Lobby**: Wait for 3 players total
3. **Bidding**: Bid on card values, select game type
4. **Declaration**: Winner chooses game type and multipliers  
5. **Card Play**: Play tricks following Skat rules
6. **Scoring**: View results and start next hand

## Session Management

The CLI automatically saves your session to `~/.erlskat-session` to maintain state during:
- Network disconnections
- Client restarts  
- Server reconnections

Sessions expire after 24 hours.

## Connection States

- **Connecting**: Attempting to reach server
- **Connected**: Ready to play
- **Disconnected**: Lost connection, attempting reconnect
- **Game Closed**: Game ended, returning to lobby

## Troubleshooting

### Connection Issues
```bash
# Test connection with debug output
./index.js --debug --host localhost --port 8080
```

### Clear Session
```bash
rm ~/.erlskat-session
```

### Server Requirements
- Erlskat server running on specified host:port
- WebSocket endpoint available at `/ws`
- Skat session cookies supported

## Development

### Project Structure
```
cli/
├── src/
│   ├── app.js              # Main app component
│   ├── websocket.js        # WebSocket connection management
│   ├── session.js          # Session persistence
│   ├── components/         # UI components
│   └── utils/             # Utilities and message handling
└── index.js               # CLI entry point
```

### Dependencies
- `ink` - Terminal UI framework
- `react` - Component framework for ink
- `ws` - WebSocket client
- `yargs` - CLI argument parsing
- `fs-extra` - File system utilities