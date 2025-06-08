// Copyright © 2016 Alan A. A. Donovan & Brian W. Kernighan.
// License: https://creativecommons.org/licenses/by-nc-sa/4.0/

// See page 61.
//!+

// Mandelbrot emits a PNG image of the Mandelbrot fractal.
package main

import (
	"image"
	"image/color"
	"image/png"
	"log"
	"math/cmplx"
	"os"
	"sync"
)

const (
	xmin, ymin, xmax, ymax = -2, -2, +2, +2
	width, height          = 1024, 1024
)

func main() {
	// img := singleThread()
	img := workerPool(3)

	out, err := os.OpenFile("out.png", os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		log.Fatalf("%v", err)
	}
	err = png.Encode(out, img)
	if err != nil {
		log.Fatalf("%v", err)
	}
}

func singleThread() *image.RGBA {
	img := image.NewRGBA(image.Rect(0, 0, width, height))

	for py := range height {
		y := float64(py)/height*(ymax-ymin) + ymin
		for px := range width {
			x := float64(px)/width*(xmax-xmin) + xmin
			z := complex(x, y)
			// Image point (px, py) represents complex value z.
			img.Set(px, py, mandelbrot(z))
		}
	}

	return img
}

type Input struct {
	py int
}

type Output struct {
	px  int
	py  int
	val color.Color
}

func workerPool(n int) *image.RGBA {
	img := image.NewRGBA(image.Rect(0, 0, width, height))

	inputChan := make(chan Input, height)
	outputChan := make(chan Output, height)

	worker := func(jobs <-chan Input, results chan<- Output, wg *sync.WaitGroup) {
		defer wg.Done()
		for j := range jobs {
			py := j.py

			y := float64(py)/height*(ymax-ymin) + ymin
			for px := range width {
				x := float64(px)/width*(xmax-xmin) + xmin
				z := complex(x, y)

				// Image point (px, py) represents complex value z.
				results <- Output{px: px, py: py, val: mandelbrot(z)}
			}
		}
	}

	var wg sync.WaitGroup
	wg.Add(n)
	for range n {
		go worker(inputChan, outputChan, &wg)
	}

	var resWg sync.WaitGroup
	resWg.Add(1)
	// collect results in a separate goroutine
	go func(results <-chan Output, wg *sync.WaitGroup) {
		defer wg.Done()
		for r := range outputChan {
			img.Set(r.px, r.py, r.val)
		}
	}(outputChan, &resWg)

	for py := range height {
		inputChan <- Input{py: py}
	}
	close(inputChan)
	wg.Wait()
	close(outputChan)

	resWg.Wait()

	return img
}

func mandelbrot(z complex128) color.Color {
	const iterations = 200
	const contrast = 15

	var v complex128
	for n := uint8(0); n < iterations; n++ {
		v = v*v + z
		if cmplx.Abs(v) > 2 {
			return color.Gray{255 - contrast*n}
		}
	}
	return color.Black
}
