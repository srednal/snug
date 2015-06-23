package com.srednal.snug

import java.util.concurrent.locks.ReentrantReadWriteLock

object RWLock {
  def apply() = new RWLock
}

class RWLock {
  private[snug] val lock = new ReentrantReadWriteLock

  def read[A](f: => A): A = {
    lock.readLock().lock()
    try f
    finally lock.readLock().unlock()
  }

  def write[A](f: => A): A = {
    // you can not obtain a write lock while a read lock is held - even if you are the holder of the read lock (self-deadlock)
    // so let's work around that
    val readLocks = lock.getReadHoldCount
    0 until readLocks foreach { _ => lock.readLock().unlock()}
    lock.writeLock().lock()
    // now we can reo-btain those read locks and restore balance
    0 until readLocks foreach { _ => lock.readLock().lock()}

    try f
    finally lock.writeLock().unlock()
  }
}
