import React from 'react';
import UnknownCard from './UnknownCard.jsx';
import './PlayerHand.css';
import './UnknownPlayerHand.css';

const UnknownPlayerHand = ({ cardsHeld = 0, className = '' }) => {
  return (
    <div className={`unknown-player-hand ${className}`}>
      {Array.from({ length: cardsHeld }, (_, index) => (
        <div
          key={index}
          className="unknown-card-container"
          style={{
            position: 'absolute',
            left: `${index * 15}px`, // 10% of 60px card width
            zIndex: index
          }}
        >
          <UnknownCard className="unknown-card" />
        </div>
      ))}
    </div>
  );
};

export default UnknownPlayerHand;
