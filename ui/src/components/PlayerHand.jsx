import React from 'react';
import Card from './Card';
import './PlayerHand.css';

const PlayerHand = ({ 
  cards = [], 
  onCardClick, 
  validCards = [], 
  isFlipped = false,
  selectedDiscardCards = [],
  isDiscardMode = false,
  selectedCardPlay = null,
  isCardPlayMode = false
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
          isSelected={selectedDiscardCards.includes(index) || selectedCardPlay === index}
          isDiscardMode={isDiscardMode}
          isCardPlayMode={isCardPlayMode}
          isInvalid={isCardPlayMode && !validCards.includes(index)}
        />
      ))}
    </div>
  );
};

export default PlayerHand; 