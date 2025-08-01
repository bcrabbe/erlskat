import React from 'react';
import PlayerHand from './PlayerHand';
import './GameBoard.css';

const GameBoard = ({
  playerHand = [],
  leftPlayerCards = [],
  rightPlayerCards = [],
  currentTrick = [],
  onCardClick,
  validCards = []
}) => {
  return (
    <div className="game-board">
      {/* Top player (left) */}
      <div className="player-position left-player">
        <div className="player-info">Left Player</div>
        <PlayerHand
          cards={leftPlayerCards}
          isFlipped={true}
        />
      </div>

      {/* Center area for current trick */}
      <div className="center-area">
        <div className="current-trick">
          {currentTrick.map((trickCard, index) => (
            <div key={index} className="trick-card">
              <div className="card-rank">{trickCard.card.rank}</div>
              <div className={`card-suit ${trickCard.card.suit === 'hearts' || trickCard.card.suit === 'diamonds' ? 'red' : 'black'}`}>
                {trickCard.card.suit === 'hearts' ? '♥' :
                 trickCard.card.suit === 'diamonds' ? '♦' :
                 trickCard.card.suit === 'clubs' ? '♣' : '♠'}
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* Right player */}
      <div className="player-position right-player">
        <div className="player-info">Right Player</div>
        <PlayerHand
          cards={rightPlayerCards}
          isFlipped={true}
        />
      </div>

      {/* Bottom player (current player) */}
      <div className="player-position bottom-player">
        <div className="player-info">Your Hand</div>
        <PlayerHand
          cards={playerHand}
          onCardClick={onCardClick}
          validCards={validCards}
        />
      </div>
    </div>
  );
};

export default GameBoard;
