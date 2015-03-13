# network-builder : Linux Network NameSpace Builder for test

[![Hackage version](https://img.shields.io/hackage/v/network-builder.svg?style=flat)](https://hackage.haskell.org/package/network-builder)  [![Build Status](https://travis-ci.org/junjihashimoto/network-builder.png?branch=master)](https://travis-ci.org/junjihashimoto/network-builder)

network-builder makes network by using Linux Network NameSpaces.

## Getting started

Install this from Hackage.

    cabal update && cabal install network-builder

## Usage

Put network-builder.yml on current directory.
The yaml format is below.

```
nss:
- - ip: 192.168.10.1/24
    name: br1
  - - - ip: 192.168.10.2/24
        name: veth-2
      - name: server2
        nss:
        - - ip: 192.168.11.1/24
            name: br1
          - - - ip: 192.168.11.4/24
                name: veth-3
              - name: server3
                nss: []
    - - ip: 192.168.10.3/24
        name: veth-4
      - name: server4
        nss:
        - - ip: 192.168.12.1/24
            name: br1
          - - - ip: 192.168.12.4/24
                name: veth-5
              - name: server5
                nss: []

```

## Commands

### create

```
network-builder create
```

### destroy

```
network-builder destroy
```

