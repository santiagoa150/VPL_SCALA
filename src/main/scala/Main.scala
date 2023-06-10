import exercise1.Product

import java.time.{DayOfWeek, LocalDate}

object Main {
  def main(args: Array[String]): Unit = {
    var products: List[Product] = List()
    var shoppingCard: List[Product] = List()
    var runApp: Boolean = true
    while (runApp) {
      println("¿Que operación desea realizar?")

      println("=======Opciones de desarrollador=======")
      println("1. Agregar producto a la lista disponible.")
      println("2. Agregar stock a un producto.")
      println("=======================================")

      println("========Opciones del comprador========")
      println("3. Ver los productos disponibles.")
      println("4. Ver los productos de una categoría.")
      println("5. Ordenar los productos de mayor a menor cantidad.")
      println("6. Ordenar los productos de menor a mayor cantidad.")
      println("7. Ver el carrito de compra.")
      println("8. Agregar producto al carrito de compra.")
      println("9. Ver el producto más costoso del carrito.")
      println("10. Ver el total en la cuenta.")
      println("11. Comprar productos del carrito.")
      println("12. Salir.")
      println("=======================================")


      val action: Int = scala.io.StdIn.readInt()
      action match {
        case 1 => products = products :+ buildProduct(products);
        case 2 => products = addStock(products);
        case 3 =>
          println("| - - - - - - - - - - - - TODOS LOS PRODUCTOS - - - - - - - - - - - - |")
          products.foreach(p => p.print());
        case 4 => printByCategory(products);
        case 5 =>
          println("| - - - - - - - - - - - - PRODUCTOS ORDENADOS - - - - - - - - - - - - |")
          sortRecursive(products) foreach (p => p.print());
        case 6 =>
          println("| - - - - - - - - - - - - PRODUCTOS ORDENADOS - - - - - - - - - - - - |")
          products.sortWith(_.stock < _.stock).foreach(p => p.print());
        case 7 =>
          println("| - - - - - - - - - - - - CARRITO DE COMPRA - - - - - - - - - - - - |")
          shoppingCard.foreach(p => p.print());
        case 8 =>
          val SKU = this.readSKU();
          val productExists = findProduct(products, SKU)
          if (!productExists) println("¡Producto no encontrado!")
          else {
            val quantity = readQuantityToRemoveFromStock()
            products = removeStock(products, SKU, quantity)
            val productInCard = findProduct(shoppingCard, SKU)
            if (productInCard) shoppingCard = addStockToCard(shoppingCard, SKU, quantity)
            else shoppingCard = shoppingCard :+ addToCard(products, SKU, quantity)
          }
        case 9 =>
          val mostExpensive = shoppingCard.sortWith((a, b) => a.stock * a.price > b.stock * b.stock).head
          if (List(mostExpensive).isEmpty) println("¡El carrito está vacío!")
          else {
            println("| - - - - - - - - - - - - EL PRODUCTO MÁS CARO - - - - - - - - - - - - |")
            mostExpensive.printWithAllValue()
          }
        case 10 =>
          val total: Double = getTotal(shoppingCard)
          println(s"| - - - - - - - - - - - - TOTAL EN LA CUENTA: ${total} - - - - - - - - - - - - |")
        case 11 =>
          val today = LocalDate.now
          val dayOfWeek = today.getDayOfWeek
          shoppingCard.foreach(p => p.printPurchase(dayOfWeek))
        case 12 => runApp = false;
        case _ => println("Opción Invalida");
      }
      println("")
    }
  }

  //UTILS
  def findProduct(products: List[Product], SKU: String): Boolean = {
    products.exists(p => p.SKU == SKU)
  }

  def getTotal(shoppingCard: List[Product]): Double = {
    val today = LocalDate.now
    val dayOfWeek = today.getDayOfWeek
    var total: Double = 0
    shoppingCard.foreach(p => {
      if (dayOfWeek != DayOfWeek.TUESDAY && p.category.toLowerCase() != "mascotas") total = total + (p.stock * p.price)
      else total = total + ((p.stock * p.price) - (p.stock * p.price * 0.2))
    })
    total
  }

  def readSKU(): String = {
    println("Ingrese el SKU del producto:")
    scala.io.StdIn.readLine()
  }

  def readQuantityToRemoveFromStock(): Int = {
    var quantity: Int = 0
    do {
      println("Ingrese la cantidad para agregar al carrito: ")
      quantity = scala.io.StdIn.readInt()
    } while (quantity <= 0)
    quantity
  }

  def printByCategory(products: List[Product]): Unit = {
    println("Ingrese la categoría para filtrar: ")
    val category: String = scala.io.StdIn.readLine()
    println(s"| - - - - - - - - - - - - ${category.toUpperCase()} - - - - - - - - - - - - |")
    products.foreach(p => if (p.category.toLowerCase() == category.toLowerCase()) p.print())
  }

  //UPDATE SHOPPING CARD
  def addToCard(products: List[Product], SKU: String, quantity: Int): Product = {
    val stockProduct = products.find(_.SKU == SKU).getOrElse(new Product("", 0, "", "", 0))
    new Product(stockProduct.name, quantity, stockProduct.SKU, stockProduct.category, stockProduct.price)
  }

  def addStockToCard(card: List[Product], SKU: String, quantity: Int): List[Product] = {
    var newProducts: List[Product] = List()
    card.foreach(p => {
      if (p.SKU == SKU) newProducts = newProducts :+ new Product(p.name, p.addStock(quantity), p.SKU, p.category, p.price)
      else newProducts = newProducts :+ new Product(p.name, p.stock, p.SKU, p.category, p.price)
    })
    newProducts
  }

  //UPDATE PRODUCTS STOCK
  def removeStock(products: List[Product], SKU: String, quantity: Int): List[Product] = {
    var newProducts: List[Product] = List()
    products.foreach(p => {
      if (p.SKU == SKU) {
        if (p.stock < quantity) println("¡El producto no tiene stock suficiente!")
        else newProducts = newProducts :+ new Product(p.name, p.removeStock(quantity), p.SKU, p.category, p.price)
      } else newProducts = newProducts :+ new Product(p.name, p.stock, p.SKU, p.category, p.price)
    })
    newProducts
  }

  def addStock(products: List[Product]): List[Product] = {
    var productWasFound: Boolean = false
    println("Ingrese el SKU del producto: ")
    val SKU: String = scala.io.StdIn.readLine()

    var stock: Int = 0
    do {
      println("Ingrese la cantidad de stock para agregar: ")
      stock = scala.io.StdIn.readInt()
    } while (stock <= 0)

    var newProducts: List[Product] = List()
    products.foreach(p => {
      if (p.SKU == SKU) {
        productWasFound = true
        newProducts = newProducts :+ new Product(p.name, p.addStock(stock), p.SKU, p.category, p.price)
        println("¡Producto agregado con exito!")
      } else newProducts = newProducts :+ new Product(p.name, p.stock, p.SKU, p.category, p.price)
    })

    if (!productWasFound) println("¡No se encontró el producto!")
    newProducts
  }

  //BUILD PRODUCTS
  def buildProduct(products: List[Product]): Product = {
    println("Ingrese el SKU del producto: ")
    var reRunSKU: Boolean = true
    var SKU: String = ""
    while (reRunSKU) {
      SKU = scala.io.StdIn.readLine()
      if (!products.exists(p => p.SKU == SKU)) reRunSKU = false
      else {
        println("El producto que deseas ingresar ya existe, por favor ingresa un código Invalido")
      }
    }
    println("Ingrese el nombre del producto: ")
    val name: String = scala.io.StdIn.readLine()

    var stock: Int = 0
    do {
      println("Ingrese el stock del producto: ")
      stock = scala.io.StdIn.readInt()
    } while (stock <= 0)

    println("Ingrese la categoría del producto: ")
    val category: String = scala.io.StdIn.readLine()

    var price: Double = 0
    do {
      println("Ingrese el precio del producto: ")
      price = scala.io.StdIn.readDouble()
    } while (price <= 0)
    new Product(name, stock, SKU, category, price)
  }

  // RECURSIVE FUNCTIONS
  def sortRecursive(products: List[Product]): List[Product] = {
    if (products.isEmpty) products
    else insert(products.head, sortRecursive(products.tail))
  }

  def insert(product: Product, products: List[Product]): List[Product] = {
    if (products.isEmpty) List(product)
    else {
      if (product.stock >= products.head.stock) product :: products
      else products.head :: insert(product, products.tail)
    }
  }
}