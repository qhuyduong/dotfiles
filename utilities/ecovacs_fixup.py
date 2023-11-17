"""Modify an HTTP form submission."""
from mitmproxy import http
import json

def response(flow: http.HTTPFlow) -> None:
    if "/api/appsvr/app.do" in flow.request.url:
        request = json.loads(flow.request.content.decode())
        if request["todo"] == "GetGlobalDeviceList":
            response = json.loads(flow.response.content.decode())
            devices = response["devices"]
            for index, _ in enumerate(devices):
                devices[index]["did"] = "17805bc5-a2b1-434d-844c-c94d3b36c4ba"
            response["devices"] = devices
            flow.response.content = json.dumps(response).encode()
