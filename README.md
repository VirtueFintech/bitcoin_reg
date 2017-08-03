# BTC Registration JSON API

This is a simple application for exposing JSON API for Bitcoin registration.

## How to Run
### Pre-Requisite
Things needed to be installed:
- Erlang 
- Make

### Compiling
1. Clone this repository
2. cd into the cloned directory
3. execute `make run`

# Documentation

 | API Method | Description | HTTP Method | URI |
 | --- | --- | --- | --- |
 | Register Bitcoin | Bitcoin SUbscription | POST | http://localhost:8080/api/v1/register


## Bitcoin Registration

Data in `JSON` form shall be like:
```
{
  "btc_address": "1HZwkjkeaoZfTSaJxDw6aKkxp45agDiEzN",
  "btc_tx_hash": "ea37f19283f53bc369f02495c47919300bb6f821a5810d7b88eabe3a5a0a1dcc",
  "eth_address": "0x2Fa9F60eF6F0FAC7af9411465CdBD10E118eEC0c",
  "btc_sig": "1HZwkjkeaoZfTSaJxDw6aKkxp45agDiEzN
G2ix6KASiUdXPiIZ+xno56Jf2brOjZZDfJP9CVkLqXUgbZXeSe9EncdBG8WUneIqGSxik/f5U5xtVBfz6if2zec=",
  "contact": "foo@gmail.com",
  "referrer": "bar@gmail.com"
}
```
