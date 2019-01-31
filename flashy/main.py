import numpy as np
import imageio
from skimage import transform
from scipy import ndimage
import sys
import time

def output_image(reg_img, fname='/dev/stdout'):
    width = 45
    height = 35
    maxval = 255
    ppm_header = f'P6 {width} {height} {maxval}\n'
    out_image = np.clip(transform.resize(reg_img, (height, width)), 0, 1)
    sys.stdout.buffer.write(bytearray(ppm_header, 'ascii'))
    sys.stdout.buffer.write(np.uint8(out_image * (maxval)).tobytes())

bigcow = np.float64(imageio.imread('cow.jpg'))/256
temp_image = bigcow

amt = 0.3
k1 = np.array([
    [
        [0], 
        [-amt], 
        [0]
    ],
    [
        [-amt],
        [1 + amt * 4],
        [-amt]
    ],
    [
        [0],
        [-amt],
        [0]
    ],
])

amt2 = amt * 0.9
k2 = np.array([
    [
        [0], 
        [amt2], 
        [0]
    ],
    [
        [amt2],
        [1 - amt2 * 4],
        [amt2]
    ],
    [
        [0],
        [amt2],
        [0]
    ],
])

for i in range(10000):
    time.sleep(0.05)
    if i % 20 > 10:
        kernel = k1
    else:
        kernel = k2
    temp_image = ndimage.convolve(temp_image, kernel, mode='reflect')
    output_image(temp_image)
