import React from 'react';

const GameValueWorking = ({ gameValueDetails, gameResult }) => {
  if (!gameValueDetails || !gameResult) return null;
  
  const {
    base_value: baseValue,
    tops_count: topsCount, 
    tops_description: topsDescription,
    multiplier: totalMultiplier,
    value: calculatedValue
  } = gameValueDetails;
  
  const {
    game_type: gameType,
    is_hand_game: isHandGame,
    selected_multipliers: selectedMultipliers,
    final_bid: finalBid,
    declarer_won: declarerWon,
    actual_game_value: actualGameValue
  } = gameResult;

  // Calculate individual multiplier components
  const baseMultiplier = 1;
  const handMultiplier = isHandGame ? 1 : 0;
  
  // Calculate bonus multipliers
  const schneiderAnnounced = selectedMultipliers.includes('schnieder');
  const schwarzAnnounced = selectedMultipliers.includes('schwartz');
  const ouvertAnnounced = selectedMultipliers.includes('ouvert');
  
  return (
    <div className="game-value-working">
      <h3>Game Value Calculation</h3>
      <div className="calculation-breakdown">
        <div className="base-value">
          <strong>Base Value:</strong> {gameType.charAt(0).toUpperCase() + gameType.slice(1)} = {baseValue}
        </div>
        <div className="multiplier-breakdown">
          <strong>Multiplier Calculation:</strong>
          <div className="multiplier-components">
            <div>Base: {baseMultiplier}</div>
            <div>{topsDescription}: +{topsCount}</div>
            {isHandGame && <div>Hand Game: +{handMultiplier}</div>}
            {schneiderAnnounced && <div>Schneider Announced: +1</div>}
            {schwarzAnnounced && <div>Schwarz Announced: +1</div>}
            {ouvertAnnounced && <div>Ouvert: +6</div>}
          </div>
          <div className="total-multiplier">
            <strong>Total Multiplier: {totalMultiplier}</strong>
          </div>
        </div>
        <div className="final-calculation">
          <strong>Game Value: {baseValue} × {totalMultiplier} = {calculatedValue}</strong>
        </div>
        <div className="bid-comparison">
          <strong>Bid: {finalBid}</strong>
          <span className={declarerWon ? "won" : "lost"}>
            {declarerWon ? " ✓ Made bid" : " ✗ Failed bid"}
          </span>
        </div>
        <div className="final-score">
          <strong>Final Score: {actualGameValue}</strong>
        </div>
      </div>
    </div>
  );
};

export default GameValueWorking;