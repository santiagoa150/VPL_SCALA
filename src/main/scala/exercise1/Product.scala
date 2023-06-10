package exercise1

import java.time.DayOfWeek

class Product(val name: String, var stock: Int, val SKU: String, val category: String, val price: Double) {

  def addStock(value: Int): Int = {
    this.stock + value
  }

  def removeStock(value: Int): Int = {
    this.stock - value
  }

  def print(): Unit = {
    println(s" - PRODUCTO: ${this.name}, STOCK: ${this.stock}, SKU: ${this.SKU}, CATEGORY: ${this.category}, PRICE: ${this.price}")
  }

  def printWithAllValue(): Unit = {
    println(s" - PRODUCTO: ${this.name}, STOCK: ${this.stock}, SKU: ${this.SKU}, CATEGORY: ${this.category}, INDIVIDUAL PRICE: ${this.price}, ALL PRICE: ${this.stock * this.price}")
  }

  def printPurchase(dayOfWeek: DayOfWeek): Unit = {
    var total: Double = 0
    if (dayOfWeek != DayOfWeek.TUESDAY && this.category.toLowerCase() != "mascotas") total = total + (this.stock * this.price)
    else total = total + ((this.stock * this.price) - (this.stock * this.price * 0.2))
    println(s"Se han comprado ${this.stock} ${this.name} productos a un valor unitario ${this.price}, para un total de: ${total}")
  }
}