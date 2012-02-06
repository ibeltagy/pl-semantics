package utcompling.mlnsemantics.data

trait DataReader[T] {

    def read(): Iterator[T]

}
