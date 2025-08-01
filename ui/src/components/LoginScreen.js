import React, { useState } from 'react';
import './LoginScreen.css';

const LoginScreen = ({ onLogin }) => {
  const [playerName, setPlayerName] = useState('');

  const handleSubmit = (e) => {
    e.preventDefault();
    if (playerName.trim()) {
      onLogin(playerName.trim());
    }
  };

  return (
    <div className="login-screen">
      <div className="login-content">
        <h1>Welcome to Erlskat</h1>
        <p>Enter your name to join the game</p>
        
        <form onSubmit={handleSubmit} className="login-form">
          <input
            type="text"
            value={playerName}
            onChange={(e) => setPlayerName(e.target.value)}
            placeholder="Enter your name"
            className="name-input"
            autoFocus
            required
          />
          <button type="submit" className="login-button" disabled={!playerName.trim()}>
            Join Game
          </button>
        </form>
      </div>
    </div>
  );
};

export default LoginScreen; 