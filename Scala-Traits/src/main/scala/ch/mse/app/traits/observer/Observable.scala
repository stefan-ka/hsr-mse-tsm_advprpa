package ch.mse.app.traits.observer

trait Observable {

  private var observers: List[Observer] = Nil;

  def addObserver(o: Observer) = {
    observers = o :: observers
  }

  def removeObserver(o : Observer) = {
    observers = observers.filterNot(_ == o)
  }

  def notifyObservers() = {
    observers.foreach(o => o.update(this))
  }

}
