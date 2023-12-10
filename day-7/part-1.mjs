import * as fs from "node:fs/promises";

const input = await fs.readFile("input.txt", { encoding: "UTF-8" });
const hands = input
  .split("\n")
  .filter((hand) => hand.length !== 0)
  .map((hand) => {
    const split = hand.split(" ");
    return [split[0], parseInt(split[1], 10)];
  })
  .sort(handOrder);
const winnings = hands
  .map(([, bet], i) => bet * (i + 1))
  .reduce((value1, value2) => value1 + value2);
console.log(winnings);

function handOrder([hand1], [hand2]) {
  const handType1 = handType(hand1);
  const handType2 = handType(hand2);
  if (handType1 < handType2) return -1;
  if (handType1 > handType2) return 1;
  // Compare based on card order
  const cardRanks = "23456789TJQKA";
  for (let i = 0; i < 5; i++) {
    const rank1 = cardRanks.indexOf(hand1[i]);
    const rank2 = cardRanks.indexOf(hand2[i]);
    if (rank1 < rank2) return -1;
    if (rank1 > rank2) return 1;
  }
  return 0;
}

function handType(hand) {
  const countByCard = new Map();
  for (const card of hand) {
    countByCard.set(card, (countByCard.get(card) ?? 0) + 1);
  }
  const counts = [...countByCard.values()].sort(
    (count1, count2) => count2 - count1,
  );

  if (counts.length === 1) {
    return 6; // Five of a kind
  }
  if (counts.length === 2) {
    // Counts must be either 4, 1 or 3, 2
    if (counts[0] === 4) {
      return 5; // Four of a kind
    } else {
      return 4; // Full house
    }
  }
  if (counts.length === 3) {
    // Counts must be either 3, 1, 1 or 2, 2, 1
    if (counts[0] === 3) {
      return 3; // Three of a kind
    } else if (counts[1] === 2) {
      return 2; // Two pair
    }
  }
  if (counts[0] === 2) {
    return 1; // One pair
  }
  return 0; // High card
}
