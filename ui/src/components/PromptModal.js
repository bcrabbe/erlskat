import React from 'react';
import './PromptModal.css';

const PromptModal = ({ 
  isOpen, 
  message, 
  choices = [], 
  onChoice, 
  onClose,
  type = 'choice'
}) => {
  if (!isOpen) return null;

  const handleChoice = (choice) => {
    onChoice(choice);
    onClose();
  };

  const handleClose = () => {
    if (onClose) {
      onClose();
    }
  };

  return (
    <div className="modal-overlay" onClick={handleClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h3>{type === 'bid_prompt' ? 'Bidding' : 
               type === 'card_play_prompt' ? 'Play a Card' : 
               'Game Prompt'}</h3>
          <button className="close-button" onClick={handleClose}>×</button>
        </div>
        
        <div className="modal-body">
          <p className="message">{message}</p>
          
          <div className="choices">
            {choices.map((choice, index) => (
              <button
                key={index}
                className="choice-button"
                onClick={() => handleChoice(choice)}
              >
                {typeof choice === 'string' ? choice : choice.toString()}
              </button>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default PromptModal; 