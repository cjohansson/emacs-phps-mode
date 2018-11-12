<?php
/**
 * Some comments here
 * @todo was here
 */

class MyClass {

    /**
     * @var string
     */
    private $var = 'abc';

    public function myMethod()
    {
        echo "Some stuff here"; // Just a comment
    }

    public function myMethod2() {
       echo "Some stuff here 2";
    }

    public function myMethod3() {
        if (!empty($this->var)) {
            $this->var = '';
        }
        $this->var = 'abc';
        $this->var = '\\';
    }

    public function heres_more_information() {
        $var = '23ac';
        $var2 = '123';
    }

}
