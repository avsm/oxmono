// SPDX-License-Identifier: ISC
package main

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"reflect"

	"tangled.org/core/api/tangled"
)

func main() {
	var fixture struct {
		Frames []string `json:"frames"`
	}
	b, err := os.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(b, &fixture); err != nil {
		panic(err)
	}
	for _, encoded := range fixture.Frames {
		wire, err := hex.DecodeString(encoded)
		if err != nil {
			panic(err)
		}
		reader := bytes.NewReader(wire)
		var event tangled.CiSubscribePipelineLogs_Event
		if err := event.Deserialize(reader); err != nil {
			panic(err)
		}
		if reader.Len() != 0 {
			panic("trailing bytes")
		}
		var out bytes.Buffer
		if err := event.Serialize(&out); err != nil {
			panic(err)
		}
		var again tangled.CiSubscribePipelineLogs_Event
		if err := again.Deserialize(&out); err != nil {
			panic(err)
		}
		if !reflect.DeepEqual(event, again) {
			panic("log event changed on roundtrip")
		}
	}
	fmt.Printf("PASS: upstream Tangled decoded and re-encoded %d OCaml log frames\n", len(fixture.Frames))
}
