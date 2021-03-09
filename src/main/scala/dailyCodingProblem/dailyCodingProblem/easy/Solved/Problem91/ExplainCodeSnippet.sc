/**
 * This problem was asked by Dropbox.
 *
 * What does the below code snippet print out? How can we fix the anonymous functions to behave as we'd expect?
 *
 * functions = []
 * for i in range(10):
 * functions.append(lambda : i)
 *
 * for f in functions:
 * print(f())
 *
 * */

val functions = Array()
val i = Range(0,10).toList
functions :++ i
for(f<-functions) yield println(f)
