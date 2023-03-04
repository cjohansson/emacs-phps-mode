# TODO

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
* Implement `xref-find-definitions` functionality
* Implement eldoc support for built-in functions
* Implement eldoc support for functions in code

## Other

* Support for mmm-mode or similar?

* Failing parse of

<?php
trait Foo
{
    public const CONSTANT = 1;
}

class Bar
{
    use Foo;
}

var_dump(Bar::CONSTANT); // 1
var_dump(Foo::CONSTANT); // Error