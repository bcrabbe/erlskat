import React from 'react';
import './LobbyScreen.css';

const LobbyScreen = ({ players = [], onStartGame }) => {
  return (
    <div className="lobby-screen">
      <div className="lobby-content">
        <h1>Waiting for Players</h1>
        <div className="player-list">
          <h2>Connected Players ({players.length}/3)</h2>
          {players.map((player, index) => (
            <div key={player.id || index} className="player-item">
              <div className="player-avatar">👤</div>
              <div className="player-name">{player.name || `Player ${index + 1}`}</div>
            </div>
          ))}
          {players.length < 3 && (
            <div className="waiting-message">
              Waiting for {3 - players.length} more player{3 - players.length !== 1 ? 's' : ''}...
            </div>
          )}
        </div>

        {players.length === 3 && (
          <div className="game-ready">
            <p>All players connected! Game will start soon...</p>
          </div>
        )}
      </div>
    </div>
  );
};

export default LobbyScreen;
