# TODO

## Parser

## Indentation

            'language' =>
                self::getLocale(),
                'recipient' => [
                    'countryCode' =>
                        $order->get_shipping_country()
                        ? $order->get_shipping_country()
                        : $order->get_billing_country(),
                        'postalCode' =>
                            $order->get_shipping_postcode()
                            ? $order->get_shipping_postcode()
                            : $order->get_billing_postcode(),
                            'type' =>
                                $order->get_shipping_company()
                                || $order->get_billing_company()
                                ? 'business'
                                : 'personal'
                ]


## Code intelligence

* Bookkeeping of chained object operators like WC()->cart->subtotal

## Other

* Support for mmm-mode or similar?
* Build in eldoc support like php-eldoc?
