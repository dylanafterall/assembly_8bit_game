## \*** This Project is Archived \***

<p>Through this project, I gained newfound appreciation for the techniques used in 8-bit games programming. Although I'm not certain how much of this is directly applicable to modern day game development, I still loved getting 
to use assembly code for such a fun application. It's possible that I might revisit this game in the future, but for now 
I've moved on to other projects.</p>

# 6502 Assembly Game Project

<p>This project uses code found in Gustavo Pezzi's (highly recommended) "Atari 2600 Programming with 6502 Assembly" course, found here: 
<a href="https://pikuma.com/courses">Pikuma Courses</a></p>

<p>Features implemented:
<ul>
    <li>Simultaneous joystick input for two players (multiplayer)</li>
    <ul>
        <li>Default Stella keybindings for Player 0: Up, Down, Left, Right</li>
        <li>Default Stella keybindings for Player 1: Y, H, G, J</li>
        <li>Stella keybinds can be remapped in options menu</li>
    </ul>
    <li>Collision detection between Player 0, Player 1, and platforms</li> 
    <li>Unique sprites for both players when stationary, running, jumping</li>
</ul>
</p>

<p>Features not implemented:
<ul>
    <li>Combat/Projectiles</li>
    <li>Scoreboard</li>
    <li>Win criteria</li>
    <li>Audio</li>
</ul>
</p>

## Dependencies:

<p>
<ul>
    <li><a href="https://dasm-assembler.github.io/">DASM</a> assembler:</li>
    <ul>
        <li>A versatile macro assembler with support for several 8-bit microprocessors</li>
    </ul>
    <li><a href="https://stella-emu.github.io/">Stella</a> emulator: </li>
    <ul>
        <li>A multi-platform Atari 2600 VCS emulator released under the GNU General Public License (GPL)</li>
    </ul>
</ul>
</p>

## Tools Used:

<p>
<ul>
    <li>Vim, Ubuntu, MacOS</li>
</ul>
</p>
