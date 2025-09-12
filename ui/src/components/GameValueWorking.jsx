import React from 'react';

const GameValueWorking = ({ gameValueDetails, gameResult, players }) => {
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
    actual_game_value: actualGameValue,
    declarer_id: declarerId,
    declarer_points: declarerPoints,
    defender_points: defenderPoints
  } = gameResult;

  // Find declarer name
  const declarer = players?.find(p => p.id === declarerId);
  const declarerName = declarer ? declarer.name : 'Unknown Player';

  // Calculate individual multiplier components
  const baseMultiplier = 1;
  const handMultiplier = isHandGame ? 1 : 0;

  // Calculate bonus multipliers
  const schneiderAnnounced = selectedMultipliers.includes('schnieder');
  const schwarzAnnounced = selectedMultipliers.includes('schwartz');
  const ouvertAnnounced = selectedMultipliers.includes('ouvert');

  // Calculate achieved bonuses
  const schneiderAchieved = defenderPoints <= 30;
  const schwarzAchieved = defenderPoints === 0;

  return (
    <div className="game-value-working">
      <div className="calculation-breakdown">
        {/* Game summary */}
        <div className="game-summary">
          <strong>{declarerName} played {gameType.charAt(0).toUpperCase() + gameType.slice(1)} (base value: {baseValue})</strong>
        </div>

        {/* Point breakdown */}
        <div className="points-breakdown">
          <div>Declarer won: {declarerPoints}</div>
          <div>Defenders won: {defenderPoints}</div>
        </div>

        {/* Multiplier calculation */}
        <div className="multiplier-breakdown">
          <strong>Multiplier Calculation:</strong>
          <div className="multiplier-components">
            <div>{topsDescription}: +{topsCount}</div>
            {isHandGame && <div>Hand: +1</div>}
            {schneiderAnnounced && <div>Schneider declared: +1</div>}
            {schneiderAchieved && !schneiderAnnounced && <div>Schneider achieved: +1</div>}
            {schwarzAnnounced && <div>Schwarz declared: +1</div>}
            {schwarzAchieved && !schwarzAnnounced && <div>Schwarz achieved: +1</div>}
            {ouvertAnnounced && <div>Ouvert: +6</div>}
          </div>
          <div className="total-multiplier">
            <strong>Total Multiplier: {totalMultiplier}</strong>
          </div>
        </div>

        {/* Game value calculation */}
        <div className="game-value-calculation">
          <strong>Total Game Value: {baseValue} × {totalMultiplier} = {calculatedValue}</strong>
        </div>

        {/* Bid result */}
        <div className="bid-result">
          <div className="bid-info">
            <strong>Declarer bid: {finalBid}, and played at {calculatedValue}</strong>
          </div>
          <div className={declarerWon ? "bid-status won" : "bid-status lost"}>
            {declarerWon ? "✓ Made bid" : "✗ Failed bid"}
          </div>
          {!declarerWon && (
            <div className="penalty-calculation">
              <strong>Points = -{calculatedValue} × 2 = {actualGameValue}</strong>
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default GameValueWorking;
