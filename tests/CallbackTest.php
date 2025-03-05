<?php

/*
 * @description Dice - A minimal Dependency Injection Container for PHP
 * @author Tom Butler tom@r.je
 * @copyright 2012-2018 Tom Butler <tom@r.je> | https:// r.je/dice.html
 * @license http:// www.opensource.org/licenses/bsd-license.php BSD License
 * @version 3.0
 */
class CallbackTest extends DiceTest
{
    public function testCallback()
    {
        $_dice            = null;
        $_args            = null;
        $rule             = [];
        $rule['callback'] = function (\Dice\Dice $dice, array $args) use (&$_dice, &$_args) {
            $_dice = $dice;
            $_args = $args;
            $obj   = new MyObj();
            $obj->setFoo(__CLASS__);
            return $obj;
        };
        $dice             = $this->dice->addRule('MyObj', $rule);
        $object           = $dice->create('MyObj', $args = ['foo' => 'bar']);
        $this->assertSame(__CLASS__, $object->getFoo());
        $this->assertSame($dice, $_dice);
        $this->assertSame($args, $_args);
    }
}
