package main

import "testing"

func TestSingleThread(t *testing.T) {
	_ = singleThread()
}

func TestWorkerPool3(t *testing.T) {
	_ = workerPool(3)
}

func TestWorkerPool5(t *testing.T) {
	_ = workerPool(5)
}

func TestWorkerPool10(t *testing.T) {
	_ = workerPool(10)
}

func TestWorkerPool100(t *testing.T) {
	_ = workerPool(100)
}

func TestWorkerPool1000(t *testing.T) {
	_ = workerPool(1000)
}
