from autobus2 import Bus

def main():
    with Bus() as bus:
        with bus.get_service_proxy({"type": "examples.hello"}) as service:
            service.wait_for_bind(timeout=2)
            print service["hi"]("great big round world")
