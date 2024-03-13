"""Modify an HTTP form submission."""
from mitmproxy import http
import json

payload = '''
{
    "code": 0,
    "devices": [
        {
            "UILogicId": "DX_AI",
            "bindTs": 1710300981534,
            "class": "bs40nz",
            "company": "eco-ng",
            "deviceName": "DEEBOT T8 AIVI",
            "did": "17805bc5-a2b1-434d-844c-c94d3b36c4ba",
            "homeId": "64194b9418ef68edf3f21ba0",
            "homeSort": 1,
            "icon": "https://portal.ecouser.net/api/pim/file/get/604efc0df49feb0008d0c93e",
            "materialNo": "110-1913-0002",
            "model": "DXAI",
            "name": "E04L13731C09HHRP0343",
            "nick": null,
            "ota": true,
            "otaUpgrade": {},
            "pid": "5de0d6658c8297000142b807",
            "product_category": "DEEBOT",
            "resource": "XIYi",
            "scode": {
                "battery": true,
                "charge": true,
                "chargestate": true,
                "clean": true,
                "tmallstand": false,
                "video": true
            },
            "service": {
                "jmq": "jmq-ngiot-cn.dc.cn.ecouser.net",
                "mqs": "api-ngiot.dc-cn.cn.ecouser.net"
            },
            "shareable": false,
            "sharedDevice": false,
            "status": 1,
            "updateInfo": {
                "changeLog": "",
                "needUpdate": false
            }
        }
    ],
    "ret": "ok",
    "todo": "result"
}'''

def response(flow: http.HTTPFlow) -> None:
    if "/api/appsvr/app.do" in flow.request.url:
        request = json.loads(flow.request.content.decode())
        if request["todo"] == "GetGlobalDeviceList":
            flow.response.content = payload.encode()
