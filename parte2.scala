
class Pelicula(
  val titulo: String,
  val duracionMinutos: Int,
  val clasificacion: String
)

object Pelicula {
  def apply(titulo: String, duracionMinutos: Int, clasificacion: String): Pelicula = {
    if duracionMinutos <= 0 then {
      new Pelicula(titulo, 90, clasificacion)
    } else {
      new Pelicula(titulo, duracionMinutos, clasificacion)
    }
  }
}


class FuncionCine(
  val pelicula: Pelicula,
  val sala: Int,
  val hora: String,
  val precioEntrada: Double,
  val asientosDisponibles: Int
) {
  
  def puedeReservar(cantidad: Int): Boolean = {
    cantidad > 0 && cantidad <= asientosDisponibles
  }

  def costoReserva(cantidad: Int): Double = {
    cantidad * precioEntrada
  }

  def reservar(cantidad: Int): FuncionCine = {
    if puedeReservar(cantidad) then {
      new FuncionCine(
        this.pelicula, 
        this.sala, 
        this.hora, 
        this.precioEntrada, 
        this.asientosDisponibles - cantidad
      )
    } else {
      new FuncionCine(
        this.pelicula, 
        this.sala, 
        this.hora, 
        this.precioEntrada, 
        this.asientosDisponibles
      )
    }
  }

  def descripcion: String = {
    s"${pelicula.titulo}\n" +
    s"Sala: $sala\n" +
    s"Hora: $hora\n" +
    s"Precio: $precioEntrada Bs\n" +
    s"Asientos disponibles: $asientosDisponibles"
  }
}

object FuncionCine {
  def apply(
    pelicula: Pelicula,
    sala: Int,
    hora: String,
    precioEntrada: Double,
    asientosDisponibles: Int
  ): FuncionCine = {
    
    val salaValida = if sala >= 1 then sala else 1
    val precioValido = if precioEntrada >= 0 then precioEntrada else 0.0
    val asientosValidos = if asientosDisponibles >= 0 then asientosDisponibles else 0

    new FuncionCine(pelicula, salaValida, hora, precioValido, asientosValidos)
  }

  def funcionEstandar(pelicula: Pelicula, sala: Int, hora: String): FuncionCine = {
    new FuncionCine(pelicula, sala, hora, 30.0, 50)
  }
}


object Parte2 {
  def main(args: Array[String]): Unit = {
    
    val pelicula1 = Pelicula("Dune", 155, "PG-13")

    val funcion1 = FuncionCine(
      pelicula1,
      3,
      "20:30",
      35.0,
      40
    )

    val funcion2 = FuncionCine.funcionEstandar(
      pelicula1,
      1,
      "16:00"
    )

   
    println("--- DESCRIPCIÓN ---")
    println(funcion1.descripcion)
    
    println("\n--- PUEDE RESERVAR 5? ---")
    println(funcion1.puedeReservar(5)) // true

    println("\n--- COSTO DE RESERVAR 5 ---")
    println(funcion1.costoReserva(5)) // 175.0 (5 * 35.0)

    println("\n--- REALIZANDO RESERVA ---")
    val funcionReservada = funcion1.reservar(5)

    println("Asientos de funcion1 (original): " + funcion1.asientosDisponibles) // 40
    println("Asientos de funcionReservada (nueva): " + funcionReservada.asientosDisponibles) // 35
    
    println("\n--- FUNCIÓN ESTÁNDAR ---")
    println(funcion2.descripcion)
  }
}
