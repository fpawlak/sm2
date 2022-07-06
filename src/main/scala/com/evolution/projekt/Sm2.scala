package com.evolution.projekt

import Domain.Card

object Sm2 {
  def rate(card: Card, grade: Int): Card = {
    val Card(_, _, _, n, ef, interval, scheduledFor) = card

    val (newN, newInterval): (Int, Long) = {
      if(grade >= 3) {
        if(n == 0) {
          (n + 1, 1)
        }
        else if(n == 1) {
          (n + 1, 6)
        }
        else {
          (n + 1, Math.round(interval * ef))
        }
      }
      else {
        (1, 1)
      }
    }

    val candidateForNewEF: Double =
      ef + (0.1 - (5 - grade) * (0.08 + (5 - grade) * 0.02))

    val newEF = if(candidateForNewEF < 1.3) 1.3 else candidateForNewEF

    card.copy(
      repetitions = newN,
      ef = newEF,
      interval = newInterval,
      scheduledFor = scheduledFor.plusDays(newInterval))
  }
}
