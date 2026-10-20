object Parte1 {
  
  // EJERCICIO 1 
  val duplicar: Int => Int = _ * 2
  val cuadrado: Int => Int = _ * _
  val esPar: Int => Boolean = _ % 2 == 0 

  // EJERCICIO 2 
  val cubo: Int => Int = n => n * n * n
  val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2
  val mayor: (Int, Int) => Int = (a, b) => if (a < b) then b else a
  val esMultiploDeTres: Int => Boolean = _ % 3 == 0

  // EJERCICIO 3
  def transformar(numero: Int, operacion: Int => Int): Int = {
    operacion(numero)
  }

  // EJERCICIO 4 
  val suma: (Double, Double) => Double = (a, b) => a + b
  val resta: (Double, Double) => Double = (a, b) => a - b
  val multiplicacion: (Double, Double) => Double = (a, b) => a * b
  val division: (Double, Double) => Double = (a, b) => a / b
  val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2
  def calcular(
      a: Double,
      b: Double,
      operacion: (Double, Double) => Double
  ): Double = {
    operacion(a, b)
  }
  //ejercicio 5 crear multiplicadores 
  val duplicar1: Int => Int = _ * 2
  val cuadrado1: Int => Int = n => n * n 
  val esPar: Int => Boolean = _ % 2 == 0
  val cubo1: Int => Int = n => n * n * n
  val promedio1: (Double, Double) => Double = (a, b) => (a + b) / 2
  val mayor: (Int, Int) => Int = (a, b) => if (a < b) then b else a
  val esMultiploDeTres: Int => Boolean = _ % 3 == 0

  def transformar(numero: Int, operacion: Int => Int): Int = operacion(numero)

  val suma: (Double, Double) => Double = (a, b) => a + b
  val resta: (Double, Double) => Double = (a, b) => a - b
  val multiplicacion: (Double, Double) => Double = (a, b) => a * b
  val division: (Double, Double) => Double = (a, b) => a / b
  
  def calcular(a: Double, b: Double, operacion: (Double, Double) => Double): Double = operacion(a, b)

  // --- EJERCICIO 5
  def crearMultiplicador(factor: Int): Int => Int = (x: Int) => x * factor
  val duplicar: Int => Int = crearMultiplicador(2)
  val triplicar: Int => Int = crearMultiplicador(3)
  val porDiez: Int => Int = crearMultiplicador(10)

  // --- EJERCICIO 6 ---
  def aplicarDescuento(precio: Double, porcentaje: Double): Double = precio - (precio * porcentaje)
  val descuento10: Double => Double = (precio: Double) => aplicarDescuento(precio, 0.10)
  val descuento20: Double => Double = (precio: Double) => aplicarDescuento(precio, 0.20)
  val descuento50: Double => Double = (precio: Double) => aplicarDescuento(precio, 0.50)

  // --- EJERCICIO 7 ---
  def elevar(exponente: Int)(base: Int): Int = {
    def potencia(b: Int, e: Int): Int = if e == 0 then 1 else b * potencia(b, e - 1)
    potencia(base, exponente)
  }
  val cuadrado: Int => Int = elevar(2)
  val cubo: Int => Int = elevar(3)
  val cuartaPotencia: Int => Int = elevar(4)

  // EJERCICIO 8 
  val limpiar: String => String = (texto: String) => texto.trim
  val convertirMinusculas: String => String = (texto: String) => texto.toLowerCase
  val longitud: String => Int = (texto: String) => texto.length
  val esLongitudValida: Int => Boolean = (num: Int) => num >= 5
  val validarTexto: String => Boolean = limpiar.andThen(convertirMinusculas).andThen(longitud).andThen(esLongitudValida)

  // EJERCICIO 9
  def sumar(desde: Int, hasta: Int, transformar: Int => Int): Int = {
    @tailrec
    def aux(actual: Int, acumulador: Int): Int = {
      if actual > hasta then acumulador
      else aux(actual + 1, acumulador + transformar(actual))
    }
    aux(desde, 0)
  }

  def producto(desde: Int, hasta: Int, transformar: Int => Int): Int = {
    @tailrec
    def aux(actual: Int, acumulador: Int): Int = {
      if actual > hasta then acumulador
      else aux(actual + 1, acumulador * transformar(actual))
    }
    aux(desde, 1) 
  }

  //  EJERCICIO 10 
  def transformarTexto(texto: String, transformacion: Char => Char): String = {
    if texto.isEmpty then ""
    else transformacion(texto.head) + transformarTexto(texto.tail, transformacion)
  }

  @tailrec
  def buscarCaracter(texto: String, condicion: Char => Boolean): Boolean = {
    if texto.isEmpty then false
    else if condicion(texto.head) then true
    else buscarCaracter(texto.tail, condicion)
  }

  
  def main(args: Array[String]): Unit = {
    println("--- Ejercicio 5 ---")
    println(duplicar(7))      // 14
    println(triplicar(7))     // 21
    println(porDiez(7))       // 70

    println("--- Ejercicio 6 ---")
    println(descuento10(500)) // 450.0
    println(descuento20(500)) // 400.0

    println("--- Ejercicio 7 ---")
    println(cuadrado(5))      // 25
    println(cubo(3))          // 27
    println(cuartaPotencia(2))// 16

    println("--- Ejercicio 8 ---")
    println(validarTexto("   Hola Mundo   ")) // true (10 caracteres)
    println(validarTexto("  abc  "))          // false (3 caracteres)

    println("--- Ejercicio 9 ---")
    println(sumar(1, 4, x => x))        // 10
    println(sumar(1, 3, x => x * x))    // 14
    println(producto(1, 4, x => x))     // 24

    println("--- Ejercicio 10 ---")
    println(transformarTexto("scala", c => c.toUpper)) // SCALA
    println(buscarCaracter("Scala2026", c => c.isDigit)) // true
    println(buscarCaracter("Scala", c => c == 'z'))    // false
  }
}
