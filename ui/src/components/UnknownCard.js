import React from 'react';
import './Card.css';

const UnknownCard = ({ className = '' }) => {
  const cardBackStyle = {
    background: 'linear-gradient(135deg, #1e3c72, #2a5298, #1e3c72)',
    color: 'white',
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center',
    justifyContent: 'center',
    position: 'relative',
    overflow: 'hidden'
  };

  const patternStyle = {
    position: 'absolute',
    top: 0,
    left: 0,
    width: '100%',
    height: '100%',
    backgroundImage: `
      radial-gradient(circle at 15px 15px, rgba(255,255,255,0.2) 1px, transparent 1px),
      radial-gradient(circle at 45px 15px, rgba(255,255,255,0.2) 1px, transparent 1px),
      radial-gradient(circle at 30px 30px, rgba(255,255,255,0.15) 2px, transparent 2px),
      linear-gradient(45deg, transparent 40%, rgba(255,255,255,0.1) 50%, transparent 60%),
      linear-gradient(-45deg, transparent 40%, rgba(255,255,255,0.1) 50%, transparent 60%),
      repeating-linear-gradient(0deg, transparent, transparent 2px, rgba(255,255,255,0.05) 2px, rgba(255,255,255,0.05) 4px),
      repeating-linear-gradient(90deg, transparent, transparent 2px, rgba(255,255,255,0.05) 2px, rgba(255,255,255,0.05) 4px)
    `,
    backgroundSize: '30px 30px, 30px 30px, 60px 60px, 20px 20px, 20px 20px, 100% 100%, 100% 100%'
  };

  const borderStyle = {
    position: 'absolute',
    top: '3px',
    left: '3px',
    right: '3px',
    bottom: '3px',
    border: '1px solid rgba(255,255,255,0.3)',
    borderRadius: '4px',
    zIndex: 2
  };

  const innerBorderStyle = {
    position: 'absolute',
    top: '6px',
    left: '6px',
    right: '6px',
    bottom: '6px',
    border: '1px solid rgba(255,255,255,0.2)',
    borderRadius: '2px',
    zIndex: 2
  };

  return (
    <div 
      className={`card ${className}`}
      style={cardBackStyle}
    >
      <div style={patternStyle}></div>
      <div style={borderStyle}></div>
      <div style={innerBorderStyle}></div>
      
      {/* Central medallion */}
      <div style={{
        position: 'relative',
        zIndex: 3,
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        background: 'rgba(255,255,255,0.1)',
        borderRadius: '50%',
        width: '32px',
        height: '32px',
        border: '1px solid rgba(255,255,255,0.3)'
      }}>
      </div>
      
      {/* Corner ornaments */}
      <div style={{position: 'absolute', top: '8px', left: '8px', fontSize: '6px', color: 'white', opacity: 0.7, zIndex: 3}}>♠</div>
      <div style={{position: 'absolute', top: '8px', right: '8px', fontSize: '6px', color: 'white', opacity: 0.7, zIndex: 3}}>♣</div>
      <div style={{position: 'absolute', bottom: '8px', left: '8px', fontSize: '6px', color: 'white', opacity: 0.7, zIndex: 3}}>♥</div>
      <div style={{position: 'absolute', bottom: '8px', right: '8px', fontSize: '6px', color: 'white', opacity: 0.7, zIndex: 3}}>♦</div>
      
      {/* Side ornaments */}
      <div style={{position: 'absolute', top: '50%', left: '4px', transform: 'translateY(-50%)', fontSize: '4px', color: 'white', opacity: 0.5, zIndex: 3}}>❖</div>
      <div style={{position: 'absolute', top: '50%', right: '4px', transform: 'translateY(-50%)', fontSize: '4px', color: 'white', opacity: 0.5, zIndex: 3}}>❖</div>
      <div style={{position: 'absolute', top: '20px', left: '50%', transform: 'translateX(-50%)', fontSize: '4px', color: 'white', opacity: 0.5, zIndex: 3}}>❖</div>
      <div style={{position: 'absolute', bottom: '20px', left: '50%', transform: 'translateX(-50%)', fontSize: '4px', color: 'white', opacity: 0.5, zIndex: 3}}>❖</div>
    </div>
  );
};

export default UnknownCard;
