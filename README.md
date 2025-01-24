# ft_turing

![License: BSD 3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)
![Version](https://img.shields.io/badge/version-1.0.0-orange.svg)

> Amazing digital turing machine in Haskell

---

## Introduction

ft_turing is a program that simulates a single-tape Turing machine based on a JSON description.  
Written in Haskell, it leverages the power of functional programming to ensure clean and efficient design.  
The program reads the machine's configuration, validates it, and executes the machine step-by-step while providing visual feedback

---

## Features

- Feature 1: Reads a Turing machine defined in a JSON file.
- Feature 2: Executes the Turing machine according to its design.
- Feature 3: Visualizes the machine's execution process for better understanding.

---

## Prerequisites

Before you begin, ensure you have the following installed on your system:  

[GHCup](https://www.haskell.org/ghcup/) is a versatile toolchain installer for **Haskell**.  
It helps you manage versions of GHC, Cabal, and Stack in a single, streamlined process.
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

---

## Installation

1. **Clone repository**
    ```bash
      git clone git@github.com:yusekim/ft_turing.git && cd ft_turing
    ```
2. **Build**
    ```bash
      cabal build
    ```
2. **Run**
    ```bash
      cabal run ft-turing -- [-h] jsonfile input
    ```
    * usage
      ``` bash
      positional arguments:
        jsonfile               json description of the machine
        input                  input of the machine
      
      optional arguments:
        -h, --help             show this help message and exit
      ```
---

## Example
> Example machines are located in the `./machines` as JSON files

1. `unary_sub.json`
   * Unary subtraction machine for strings consisting of only '1's
   * input example : `111-11=`
2. `unary_add.json`
   * Unary addition machine for strings consisting of only '1's
   * input example : `111+11=`
3. `palindrome.json`
   *  $\( L = \{ w \in \{ a, b \}^* \mid w = w^R \}\)$
   * input example :  `abba` or `aba`
4. `0^2n.json`
   *  $\( L = \{ 0^{2n} \mid n \geq 0, \, n \in \mathbb{N} \cup \{0\} \} \)$
   *  input example : `0000` or `000000`
5. `improved0^2n.json`
   *  $\( L = \{ 0^{2n} \mid n \geq 0, \, n \in \mathbb{N} \cup \{0\} \} \)$
   *  input example `0000` or `000000`
6. `0^n1^n.json`
   * $\(L = \{ 0^n1^n \mid n \geq 0, \, n \in \mathbb{N} \cup \{0\} \}\)$
   * input example `00001111` or `000111`

---

## Custom JSON file
> Design the machine according to the following JSON file format.
```json
{
  "name" : "machine name",
  "alphabet": [ "input alphabet symbols + blank symbol" ],
  "blank" : "blank symbol",
  "states" : [ "states" ],
  "initial" : "start state ∈ states",
  "finals" : [ "final states ∈ states" ],
  "transitions" : {
      "state_0": [
          { "read" : "read symbol", "to_state": "next state", "write": "write symbol", "action": "RIGHT or LEFT"},
      ],
      "..."
      "state_n": [
          { "read" : "read symbol", "to_state": "next state", "write": "write symbol", "action": "RIGHT or LEFT"},
      ]
  }
}

```
---
## Contributors
<table>
    <tbody>
        <tr>
            <td align="center" valign="top">
                <a href="=">
                    <img src="https://avatars.githubusercontent.com/u/127705483?v=4" width="150px;" height="150px;" alt="yusekim"/><br />
                    <sub><b>Kim Yuseung</b></sub>
                </a>
            </td>
            <td align="center" valign="top">
                <a href="=">
                    <img src="https://avatars.githubusercontent.com/u/58614643?v=4" width="150px;" height="150px;" alt="phan"/><br />
                    <sub><b>Han Pilho</b></sub>
                </a>
            </td>
        </tr>
    </tbody>
</table>
