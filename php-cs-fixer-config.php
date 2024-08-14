#!/usr/bin/php
<?php

error_reporting(E_ALL | E_STRICT);

$finder = \PhpCsFixer\Finder::create();

$config = new \PhpCsFixer\Config();
$config->setRules([
    '@Symfony' => true,
    'array_syntax' => ['syntax' => 'short'],
    'array_indentation' => true,
    'multiline_whitespace_before_semicolons' => true,
    'method_argument_space' => ['ensure_fully_multiline' => true],
    'declare_equal_normalize' => [
        'space' => 'single',
    ],
    'binary_operator_spaces' => [
        'default' => 'single_space'
    ],
    'concat_space' => ['spacing' => 'one'],
]);

return $config->setFinder($finder);
