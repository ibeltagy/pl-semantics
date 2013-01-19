package utcompling.scalalogic.util

class Counter(private val initialValue: Int = 0) {

    private var value = initialValue

    def get(): Int = {
        value += 1
        return value
    }

}
