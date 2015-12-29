package main

import (
  "fmt"
  "image"
  "image/color"
  "image/png"
  "math"
  "math/rand"
  "os"
  "time"
)

type Expr interface {
  Eval(x float64, y float64) (r, g, b float64)
}

func average(x,y,w float64) float64 {
  return w * x + (1.0 - w) * y
}

type SumExpr struct {
  e1     Expr
  e2     Expr
}

func (e SumExpr) Eval(x, y float64) (r, g, b float64) {
  r1,g1,b1 := e.e1.Eval(x,y)
  r2,g2,b2 := e.e2.Eval(x,y)
  r = average(r1,r2,0.5)
  g = average(g1,g2,0.5)
  b = average(b1,b2,0.5)
  return
}

func tent(x float64) float64 {
  return 1.0 - 2.0 * math.Abs(x)
}

type TentExpr struct {
  e1    Expr
}

func (e TentExpr) Eval(x, y float64) (r, g, b float64) {
  r1,g1,b1 := e.e1.Eval(x,y)
  r = tent(r1)
  g = tent(g1)
  b = tent(b1)
  return
}

func pown(x float64, y int) float64 {
  acc := 1.0
  for i:=0; i<y;i++ {acc *= x }
  return acc
}

func well(x float64) float64 {
  return 1.0 - 2.0 / pown(1.0 + x * x,8)
}

type WellExpr struct {
  e1    Expr
}

func (e WellExpr) Eval(x, y float64) (r, g, b float64) {
  r1,g1,b1 := e.e1.Eval(x,y)
  r = well(r1)
  g = well(g1)
  b = well(b1)
  return
}

type SinExpr struct {
  e1    Expr
  phase float64
  freq  float64
}

func makeSinExpr(e Expr) Expr {
  return SinExpr{e, rand.Float64() * math.Pi, (rand.Float64() * 5.0) + 1.0}
}

func (e SinExpr) Eval(x, y float64) (r, g, b float64) {
  r1, g1, b1 := e.e1.Eval(x, y)
  r = math.Sin(e.phase + r1*e.freq)
  g = math.Sin(e.phase + g1*e.freq)
  b = math.Sin(e.phase + b1*e.freq)
  return
}

type MaxExpr struct {
  e1 Expr
  e2 Expr
}

func (e MaxExpr) Eval(x, y float64) (r, g, b float64) {
  r1, g1, b1 := e.e1.Eval(x, y)
  r2, g2, b2 := e.e2.Eval(x, y)
  r = math.Max(r1, r2)
  g = math.Max(g1, g2)
  b = math.Max(b1, b2)
  return
}

type ModExpr struct {
  e1 Expr
  e2 Expr
}

func (e ModExpr) Eval(x, y float64) (r, g, b float64) {
  r1, g1, b1 := e.e1.Eval(x, y)
  r2, g2, b2 := e.e2.Eval(x, y)
  r = math.Mod(r1, r2)
  g = math.Mod(g1, g2)
  b = math.Mod(b1, b2)
  return
}

type SquareExpr struct {
  e1 Expr
}

func (e SquareExpr) Eval(x, y float64) (r, g, b float64) {
  r1, g1, b1 := e.e1.Eval(x, y)
  r = r1 * r1
  g = g1 * g1
  b = b1 * b1
  return
}

type ProductExpr struct {
  e1 Expr
  e2 Expr
}

func (e ProductExpr) Eval(x, y float64) (r, g, b float64) {
  r1, g1, b1 := e.e1.Eval(x, y)
  r2, g2, b2 := e.e2.Eval(x, y)
  r = r1 * r2
  g = g1 * g2
  b = b1 * b2
  return
}

type MixExpr struct {
  e1 Expr
  e2 Expr
  e3 Expr
}

func (e MixExpr) Eval(x, y float64) (r, g, b float64) {
  n, _, _ := e.e1.Eval(x, y)
  w := 0.5 * (n + 1.0)
  r1,g1,b1 := e.e2.Eval(x, y)
  r2,g2,b2 := e.e3.Eval(x, y)
  r = average(r1,r2,w)
  g = average(g1,g2,w)
  b = average(b1,b2,w)
  return
}

type LevelExpr struct {
  e1 Expr
  e2 Expr
  e3 Expr
  threshold float64
}

func (e LevelExpr) Eval(x, y float64) (r, g, b float64) {
  r1,g1,b1 := e.e1.Eval(x, y)
  r2,g2,b2 := e.e2.Eval(x, y)
  r3,g3,b3 := e.e3.Eval(x, y)
  r = r2
  if r1 < e.threshold { r = r3 }
  g = g2
  if g1 < e.threshold { g = g3 }
  b = b2
  if b1 < e.threshold { b = b3 }
  return
}

func makeLevelExpr(e1,e2,e3 Expr) Expr {
  threshold := (rand.Float64()*2.0) - 1.0
  return LevelExpr{e1,e2,e3,threshold}
}

type XExpr struct{}

func (XExpr) Eval(x, _ float64) (r, g, b float64) {
  r = x
  g = x
  b = x
  return
}

type YExpr struct{}

func (YExpr) Eval(_, y float64) (r, g, b float64) {
  r = y
  g = y
  b = y
  return
}

type ConstantExpr struct {
  r float64
  g float64
  b float64
}

func (c ConstantExpr) Eval(x, y float64) (r, g, b float64) {
  r = c.r
  g = c.g
  b = c.b
  return
}

func makeConstantExpr() Expr {
  return ConstantExpr{rand.Float64(), rand.Float64(), rand.Float64()}
}

func generateExpr(count int) Expr {
  if count == 0 {
    switch rand.Intn(3) {
    case 0: return XExpr{}
    case 1: return YExpr{}
    default: return makeConstantExpr()
    }
  }
  switch rand.Intn(10) {
  case 0:
    return MixExpr{generateExpr(count - 1), generateExpr(count - 1), generateExpr(count -1)}
  case 1:
    return ProductExpr{generateExpr(count - 1), generateExpr(count - 1)}
  case 2:
    return ModExpr{generateExpr(count - 1), generateExpr(count - 1)}
  case 3:
    return MaxExpr{generateExpr(count - 1), generateExpr(count - 1)}
  case 4:
    return SquareExpr{generateExpr(count - 1)}
  case 5:
    return WellExpr{generateExpr(count - 1)}
  case 6:
    return TentExpr{generateExpr(count - 1)}
  case 7:
    return SumExpr{generateExpr(count - 1), generateExpr(count - 1)}
  case 8:
    return makeSinExpr(generateExpr(count - 1))
  default:
    return makeLevelExpr(generateExpr(count - 1), generateExpr(count - 1), generateExpr(count -1))
  }
}

func toByte(x float64) uint8 {
  return uint8(math.Max(0.0, math.Min(255.0, 128.0*(x+1.0))))
}

func createImage(width int, height int, e *Expr) image.Image {
  imgRect := image.Rect(0, 0, width, height)
  img := image.NewRGBA(imgRect)
  for y := 0; y < height; y++ {
    for x := 0; x < width; x++ {
      u := (float64(x-width/2)+0.5)/float64(width)
      v := (float64(y-height/2)+0.5)/float64(height)
      r, g, b := (*e).Eval(u, v)
      c := color.RGBA{toByte(r), toByte(g), toByte(b), 255}
      img.SetRGBA(x, y, c)
    }
  }
  return img
}

func main() {
  rand.Seed(time.Now().UTC().UnixNano())
  for i:=100;i<=999;i++ {
    out, _ := os.Create(fmt.Sprintf("./output%d.png",i))
    defer out.Close()
    //e := makeSinExpr(ProductExpr{YExpr{}, MaxExpr{XExpr{}, makeConstantExpr()}})
    e := generateExpr(6)
    img := createImage(640, 480, &e)
    png.Encode(out, img)
  }
}
