var v = document . createElement ("p")

v . classList . add ("service")

var span = document . createElement ("span")

span . classList . add ("title")

var span2 = document . createElement ("span")

span2 . classList . add ("name")

span2 . appendChild (document . createTextNode ("wowo"))

span . appendChild (span2)

var a = document . createElement ("a")

a . classList . add ("label")

a . href = "http://tafdi.org"

a . appendChild (span)

v . appendChild (a)

document . getElementsByClassName ("services") [0] . appendChild (v)
