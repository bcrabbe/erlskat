import React, { useEffect, useCallback } from 'react';
import GameValueWorking from './GameValueWorking';
import './PromptModal.css';

const ScoresModal = ({ scoresModal, gameValueDetails, lastGameResult, players, onClose }) => {
  const handleKeyDown = useCallback((event) => {
    if (event.key === 'Enter') {
      event.preventDefault();
      onClose();
    }
  }, [onClose]);

  const handleOverlayClick = useCallback((event) => {
    if (event.target === event.currentTarget) {
      onClose();
    }
  }, [onClose]);

  useEffect(() => {
    if (scoresModal) {
      document.addEventListener('keydown', handleKeyDown);
      return () => document.removeEventListener('keydown', handleKeyDown);
    }
  }, [scoresModal, handleKeyDown]);

  if (!scoresModal) return null;

  return (
    <div className="scores-modal-overlay" onClick={handleOverlayClick}>
      <div className="scores-modal">
        <h2>Game Complete</h2>
        <div className="scores-modal-content">
          <GameValueWorking gameValueDetails={gameValueDetails} gameResult={lastGameResult} players={players} />
          <div className="scoreboard-section">
            <h3>Scoreboard</h3>
            <div className="scores-list">
              {scoresModal.playerScores.map((playerScore) => {
                const player = players.find(p => p.id === playerScore.player_id);
                const playerName = player ? player.name : 'Unknown Player';
                return (
                  <div key={playerScore.player_id} className="score-item">
                    <span className="player-name">{playerName}</span>
                    <span className="player-score">{playerScore.score}</span>
                  </div>
                );
              })}
            </div>
          </div>
        </div>
        <button 
          className="choice-button" 
          onClick={onClose}
          autoFocus
          style={{ marginTop: '20px', alignSelf: 'center', minWidth: '120px' }}
        >
          Continue
        </button>
      </div>
    </div>
  );
};

export default ScoresModal;