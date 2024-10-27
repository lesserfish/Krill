# Development Notes

#### Text mode

In text mode, each line can show up to 40 characters across 24 lines. Each character measures 5 dots in width and 7 dots in height. However, there is a 1-dot wide space on either side and a 1-dot high space above each character. This results in each character occupying 7 pixels in width and 8 pixels in height.

Thus, the resolution of the Apple II in text mode is (40 * 7) x (24 * 8) = 280 x 192 (width x height)

Text is monochrome. There are 26 upper case character, 10 digits, and 28 special characters, for a total of 64 characters. Thus, the minimum size of the ROM data is 64 * 7 * 8 bits = 3584 bits.

Since storage is not an issue anymore, for convenience, we will just represent this as 3584 bytes instead.

#### Low graphics mode



#### High graphics mode

In high resolution mode, the display is effectively 280 x 192 pixels. In high resolution mode, the display uses a $4000 byte space "Picture buffer" located in address $2000 to $3FFF. This actually contains two pages. Page one is located between $2000 and 3FFF and page 2 is located between $4000 and $5FFF.

