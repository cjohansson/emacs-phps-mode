<?php

if (true):
    echo 'was true 1';
    echo 'was true 1 2';
endif;

if (true):
    echo 'was true 2';
else:
    echo 'was false 2';
endif;


switch (true):
    case true:
        echo 'was true 3';
        echo 'was true 3 2';
        break;
    default:
        echo 'the default';
endswitch;

