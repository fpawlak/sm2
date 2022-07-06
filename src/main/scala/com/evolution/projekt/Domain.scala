package com.evolution.projekt

import java.time.LocalDate

object Domain {
  case class Card(
    id: Int,
    question: String,
    answer: String,
    repetitions: Int,
    ef: Double,
    interval: Long,
    scheduledFor: LocalDate)

  case class QuestionAnswer(
    qaQuestion: String,
    qaAnswer: String,
    qaScheduledFor: LocalDate)
}
