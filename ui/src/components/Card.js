import React from 'react';
import './Card.css';

const Card = ({ card, onClick, isPlayable = false, isFlipped = false, isSelected = false, isDiscardMode = false, isCardPlayMode = false, isInvalid = false }) => {
  if (!card) return null;

  const { rank, suit } = card;

  const getSuitSymbol = (suit) => {
    switch (suit) {
      case 'hearts': return '♥';
      case 'diamonds': return '♦';
      case 'clubs': return '♣';
      case 'spades': return '♠';
      default: return '';
    }
  };

  const getSuitColor = (suit) => {
    return suit === 'hearts' || suit === 'diamonds' ? 'red' : 'black';
  };

  const handleClick = () => {
    if ((isPlayable || isDiscardMode || (isCardPlayMode && !isInvalid)) && onClick) {
      onClick();
    }
  };

  return (
    <div
      className={`card ${isPlayable ? 'playable' : ''} ${isFlipped ? 'flipped' : ''} ${isSelected ? 'selected' : ''} ${isDiscardMode ? 'discard-mode' : ''} ${isCardPlayMode ? 'card-play-mode' : ''} ${isInvalid ? 'invalid' : ''}`}
      onClick={handleClick}
    >
      {isFlipped ? (
        <div className="card-back">
          <div className="card-pattern">♠♣♥♦</div>
        </div>
      ) : (
        <div className="card-front">
          <div className="card-rank">{rank}</div>
          <div className={`card-suit ${getSuitColor(suit)}`}>
            {getSuitSymbol(suit)}
          </div>
        </div>
      )}
    </div>
  );
};

export default Card;
