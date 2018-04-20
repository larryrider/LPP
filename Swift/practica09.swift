// Lawrence Rider García


// Practica 9

// Ejercicio 1 

func ordenado(array: [Int]) -> Bool {
    for i in 0..<array.count{
        if (i != array.count-1 && array[i] > array[i+1]){
            return false;
        }
    }
    return true;
}

let aOrdenado = [10, 20, 30]
let aDesordenado = [20, 10, 30]
print("Ejercicio 1")
print("¿El array \(aOrdenado) está ordenado?: \(ordenado(aOrdenado))")
print("¿El array \(aDesordenado) está ordenado?: \(ordenado(aDesordenado))")


// Ejercicio 2

func min (num1: Int, num2: Int) -> Int{
    if (num1 < num2){
        return num1
    }
    else{
        return num2
    }
}

func max (num1: Int, num2: Int) -> Int{
    if (num1 < num2){
        return num2
    }
    else{
        return num1
    }
}

func union(intervalo: (Int, Int), con intervalo2: (Int, Int)) -> (Int, Int){
    return (min(intervalo.0, intervalo2.0) , max(intervalo.1,intervalo2.1))
}

let intervalo1 = (4, 10)
let intervalo2 = (3, 8)
print("\nEjercicio 2")
print("La unión de \(intervalo1) y \(intervalo2) es \(union(intervalo1, con: intervalo2))")


func intersecta(intervalo: (Int, Int), con intervalo2: (Int, Int)) -> Bool{
    return (intervalo2.0 <= intervalo.1 && intervalo.0 <= intervalo2.1)
}

func interseccion(intervalo:(Int, Int), con intervalo2: (Int, Int)) -> (Int, Int)?{
    if (intersecta(intervalo,con: intervalo2)){
        return (max(intervalo.0, intervalo2.0), min(intervalo.1, intervalo2.1))
    } else{
        return nil
    }
}

let intervalo3 = (8, 15)
let intervalo4 = (12, 20)

print("La intersección de \(intervalo1) y \(intervalo3) es \(interseccion(intervalo1, con: intervalo3))")
print("La intersección de \(intervalo1) y \(intervalo4) es \(interseccion(intervalo1, con: intervalo4))")


// Ejercicio 3

func buscaValores(array: [Int], _ diccionario: [Int:String]) -> [String]{
    var stringValores = [String] ();
    for x in 0..<array.count{
        for (y,valor) in diccionario{
            if (array[x] == y){
                stringValores.append(valor)
            }
        }
    }
    
    return stringValores
}


print("\nEjercicio 3")
print(buscaValores([1,2,2,1,3], [1: "patatas", 2: "huevos", 3: "leche"]))


// Ejercicio 4

enum Respuesta: Int {
   case Nada = 0, Regular, Medio, Bastante, Todo
}

func masRepetido(diccionario: [Respuesta:Int]) -> Respuesta{
    var masrepetido = Respuesta.Todo
    var contador = 0
    
    for (respuesta, cantidad) in diccionario{
        if (cantidad > contador){
            contador = cantidad
            masrepetido = respuesta
        }
    }
    return masrepetido
}

func estadisticas(respuestas: [Respuesta]) -> (Respuesta, Double){
    var media = 0.0
    var veces: [Respuesta: Int] = [:]
    
    for x in respuestas {
        media += Double(x.rawValue)
        if (veces[x] == nil){
            veces[x] = 1
        } else{
            veces[x]! += 1
        }
    }
    media = media / Double(respuestas.count)
    
    return (masRepetido(veces), media)
}

print("\nEjercicio 4")
let contestaciones = [Respuesta.Nada, Respuesta.Nada, Respuesta.Todo, Respuesta.Medio, Respuesta.Regular]
print(estadisticas(contestaciones))



// Ejercicio 5

