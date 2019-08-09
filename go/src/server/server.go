package main

import (
    "fmt"
    "io"
    "io/ioutil"
    "net/http"
    "os"
)

const indexhtml = `
<!DOCTYPE HTML>
<html>
<head>
  <script src="main.js"></script>
</head>
<body>
  <div id="elm"></div>
  <script src="elm.js"></script>
</body>
</html>`

const elmjs = `
var app = Elm.Main.init({
    node: document.getElementById('elm')
})`

const csp = "child-src 'none'; connect-src 'self'; default-src 'none'; font-src 'self'; frame-src 'none'; img-src 'self'; manifest-src 'none'; media-src 'self'; object-src 'none'; prefetch-src 'none'; script-src 'self'; style-src 'self'; worker-src 'none'; report-uri http://localhost:3333/cspreport;"

func main() {
    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        io.WriteString(w, indexhtml)
    })
    http.HandleFunc("/elm.js", func(w http.ResponseWriter, r *http.Request) {
        io.WriteString(w, elmjs)
    })
    http.HandleFunc("/main.js", func(w http.ResponseWriter, r *http.Request) {
        f, _ := os.Open("main.js")
        io.Copy(w, f)
    })
    http.HandleFunc("/cspreport", func(w http.ResponseWriter, r *http.Request) {
        body, _ := ioutil.ReadAll(r.Body)
        fmt.Println(string(body))
    })
    fmt.Println(http.ListenAndServe(":3333", nil))
}
