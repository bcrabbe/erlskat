import React from 'react';
import UnknownCard from './UnknownCard';
import './Card.css';
import './UnknownCardPile.css';

const UnknownCardPile = ({ cardCount = 0, className = '' }) => {
  // Generate random jitter values for position and rotation
  const generateJitter = (index) => {
    // Use index as seed for consistent positioning
    const seed = index + 1;
    const random1 = (seed * 9301 + 49297) % 233280 / 233280;
    const random2 = (seed * 15485863 + 49297) % 233280 / 233280;
    const random3 = (seed * 12345 + 67890) % 233280 / 233280;
    
    return {
      x: (random1 - 0.5) * 4, // -2px to +2px
      y: (random2 - 0.5) * 4, // -2px to +2px
      rotation: (random3 - 0.5) * 6 // -3deg to +3deg
    };
  };

  return (
    <div className={`unknown-card-pile ${className}`}>
      {Array.from({ length: cardCount }, (_, index) => {
        const jitter = generateJitter(index);
        return (
          <div
            key={index}
            className="pile-card-container"
            style={{
              position: 'absolute',
              transform: `translate(${jitter.x}px, ${jitter.y}px) rotate(${jitter.rotation}deg)`,
              zIndex: index
            }}
          >
            <UnknownCard className="pile-card" />
          </div>
        );
      })}
    </div>
  );
};

export default UnknownCardPile;