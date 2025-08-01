import React from 'react';
import Card from './Card';
import './PlayerHand.css';

const PlayerHand = ({ 
  cards = [], 
  onCardClick, 
  validCards = [], 
  isFlipped = false,
  selectedDiscardCards = [],
  isDiscardMode = false
}) => {
  return (
    <div className="player-hand">
      {cards.map((card, index) => (
        <Card
          key={`${card.rank}-${card.suit}-${index}`}
          card={card}
          onClick={() => onCardClick(index)}
          isPlayable={validCards.includes(index)}
          isFlipped={isFlipped}
          isSelected={selectedDiscardCards.includes(index)}
          isDiscardMode={isDiscardMode}
        />
      ))}
    </div>
  );
};

export default PlayerHand; 