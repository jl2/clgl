#version 330 core

in vec4 Color;
out vec4 outColor;

void main()
{
    outColor = vec4(Color.r, 0, Color.g, 1.0);
}
