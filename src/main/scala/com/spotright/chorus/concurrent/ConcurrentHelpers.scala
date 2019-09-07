package com.spotright.chorus.concurrent

import java.util.concurrent.{Executors, ThreadFactory}

import org.apache.commons.lang3.concurrent.BasicThreadFactory

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

trait ConcurrentHelpers {

  def namedThreadFactor(name: String): ThreadFactory =
    new BasicThreadFactory.Builder()
      .namingPattern(s"$name-%d")
      .daemon(true)
      .build()

  def fixedPoolECES(name: String, numThreads: Int): ExecutionContextExecutorService = {
    val pool = Executors.newFixedThreadPool(numThreads, namedThreadFactor(name))
    ExecutionContext.fromExecutorService(pool)
  }
}
