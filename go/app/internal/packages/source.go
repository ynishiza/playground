package packages

import (
	"context"
	"fmt"
	"time"

	. "github.com/ynishiza/myapp/internal/utils"
)

func TestContext() {
	PrintBanner("TestContext")

	var runProcess = func(resultTime, timeout int) (result chan bool, ctx context.Context, cancel context.CancelFunc) {
		result = make(chan bool, 1)
		ctx = context.Background()
		ctx, cancel = context.WithTimeoutCause(ctx, time.Duration(timeout)*time.Second, fmt.Errorf("Timeout"))
		go func() {
			time.Sleep(time.Duration(resultTime) * time.Second)
			result <- true
		}()

		return
	}

	result, ctx, cancel := runProcess(1, 2)
	select {
	case x := <-result:
		fmt.Printf("success: %v\n", x)
	case <-ctx.Done():
		var err = ctx.Err()
		var cause = context.Cause(ctx)
		switch err {
		case context.Canceled:
			fmt.Printf("Canceled %v\n", cause.Error())
			printInfo(ctx)
		case context.DeadlineExceeded:
			fmt.Printf("DeadlineExceeded %v\n", cause.Error())
			printInfo(ctx)
		}
	}

	// cancel
	result, ctx, cancel = runProcess(1000, 1000)
	cancel()
	select {
	case x := <-result:
		fmt.Printf("%v", x)
	case <-ctx.Done():
		printInfo(ctx)
	}

	// deadline
	result, ctx, cancel = runProcess(1000000, 1)
	select {
	case x := <-result:
		fmt.Printf("%v", x)
	case <-ctx.Done():
		printInfo(ctx)
	}
}

func printInfo(ctx context.Context) {
	var err = ctx.Err()
	var cause = context.Cause(ctx)
	if err == nil {
		fmt.Printf("\n[Info] no error\n")
	} else {
		fmt.Printf("\n[Info] Error: %v \tCause: %v\tCanceled: %v \t Deadline: %v\n", err.Error(), cause.Error(), err == context.Canceled, err == context.DeadlineExceeded)
	}
}
