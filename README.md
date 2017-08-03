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
* Note: btc_sig `MUST` be base64-encoded from the signature output as the signature varies from one client to another.
```
{
  "btc_address": "1HZwkjkeaoZfTSaJxDw6aKkxp45agDiEzN",
  "btc_tx_hash": "ea37f19283f53bc369f02495c47919300bb6f821a5810d7b88eabe3a5a0a1dcc",
  "eth_address": "0x2Fa9F60eF6F0FAC7af9411465CdBD10E118eEC0c",
  "btc_sig": "MUhad2tqa2Vhb1pmVFNhSnhEdzZhS2t4cDQ1YWdEaUV6TgogIEcyaXg2S0FTaVVkWFBpSVoreG5vNTZKZjJick9qWlpEZkpQOUNWa0xxWFVnYlpYZVNlOUVuY2RCRzhXVW5lSXFHU3hpay9mNVU1eHRWQmZ6NmlmMnplPQo=",
  "contact": "foo@gmail.com",
  "referrer": "bar@gmail.com"
}
```

## Request
```
curl -H 'Content-Type: application/json' -d '{
>   "btc_address": "1HZwkjkeaoZfTSaJxDw6aKkxp45agDiEzN",
>   "btc_tx_hash": "ea37f19283f53bc369f02495c47919300bb6f821a5810d7b88eabe3a5a0a1dcc",
>   "eth_address": "0x2Fa9F60eF6F0FAC7af9411465CdBD10E118eEC0c",
>   "btc_sig": "MUhad2tqa2Vhb1pmVFNhSnhEdzZhS2t4cDQ1YWdEaUV6TgogIEcyaXg2S0FTaVVkWFBpSVoreG5vNTZKZjJick9qWlpEZkpQOUNWa0xxWFVnYlpYZVNlOUVuY2RCRzhXVW5lSXFHU3hpay9mNVU1eHRWQmZ6NmlmMnplPQo=", 
>   "contact": "foo@gmail.com",
>   "referrer": "bar@gmail.com"
> }' http://localhost:8080/api/v1/register -vvv

```

## Response
```
*   Trying ::1...
* TCP_NODELAY set
* Connection failed
* connect to ::1 port 8080 failed: Connection refused
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /api/v1/register HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.54.0
> Accept: */*
> Content-Type: application/json
> Content-Length: 452
> 
* upload completely sent off: 452 out of 452 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Thu, 03 Aug 2017 15:03:27 GMT
< content-length: 19
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
< 
* Connection #0 to host localhost left intact
{"ok":"data added"}
```

