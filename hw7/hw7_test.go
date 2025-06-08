package main

import (
	"runtime"
	"testing"
)

func BenchmarkSingleThread(b *testing.B) {
	for b.Loop() {
		singleThread()
	}
}

func BenchmarkWorkerPool3(b *testing.B) {
	for b.Loop() {
		workerPool(3)
	}
}

func BenchmarkWorkerPool5(b *testing.B) {
	for b.Loop() {
		workerPool(5)
	}
}

func BenchmarkWorkerPool10(b *testing.B) {
	for b.Loop() {
		workerPool(10)
	}
}

func BenchmarkWorkerPool100(b *testing.B) {
	for b.Loop() {
		workerPool(100)
	}
}

func BenchmarkWorkerPool1000(b *testing.B) {
	for b.Loop() {
		workerPool(1000)
	}
}
func BenchmarkWorkerPoolNumCpu(b *testing.B) {
	n := runtime.NumCPU()
	for b.Loop() {
		workerPool(n)
	}
}
