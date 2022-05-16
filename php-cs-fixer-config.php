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
    'concat_space' => ['spacing' => 'one'],
]);

return $config->setFinder($finder);
