import React, { useMemo, useRef } from 'react';
import UnknownCard from './UnknownCard.jsx';
import './Card.css';
import './UnknownCardPile.css';

const UnknownCardPile = ({ cardCount = 0, className = '' }) => {
  // Store jitter values per card index to persist across re-renders
  const jitterCache = useRef({});

  const getJitterForCard = (index) => {
    if (!jitterCache.current[index]) {
      jitterCache.current[index] = {
        x: (Math.random() - 0.5) * 30, // -15px to +15px
        y: (Math.random() - 0.5) * 30, // -15px to +15px
        rotation: (Math.random() - 0.5) * 30 // -15deg to +15deg
      };
    }
    return jitterCache.current[index];
  };

  return (
    <div className={`unknown-card-pile ${className}`}>
      {Array.from({ length: cardCount }, (_, index) => {
        const jitter = getJitterForCard(index);
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
