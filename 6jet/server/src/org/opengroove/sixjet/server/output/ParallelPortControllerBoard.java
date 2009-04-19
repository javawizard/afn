package org.opengroove.sixjet.server.output;

import parport.ParallelPort;

public class ParallelPortControllerBoard implements ControllerBoard {
	public static final int data1 = 0x01;
	public static final int data2 = 0x01 << 1;
	public static final int data3 = 0x01 << 2;
	public static final int data4 = 0x01 << 3;
	public static final int data5 = 0x01 << 4;
	public static final int data6 = 0x01 << 5;
	public static final int data7 = 0x01 << 6;
	public static final int data8 = 0x01 << 7;
	/**
	 * 0 if power is off or {@link #data6} if power is on
	 */
	public volatile int powerStatus;

	public volatile int jetState;

	public static final Object writeLock = new Object();

	public Thread powerStateThread = new Thread() {
		public void run()
		{
			while(true)
			{
				
			}
		}
	};

	/**
	 * The address of the parallel port to communicate with. 0x378 (888 in
	 * decimal) is LPT1. This will probably be configurable in the future.
	 */
	public static final int address = 0x378;

	public void flush() {
		shiftOut(jetState);
	}

	/**
	 * Sets pins 2 through 9 on the physical parallel port to be the
	 * least-significant 8 bits of the number specified. This method also
	 * inserts a delay suitable for the shift registers.
	 */
	public void write(int b) {
		System.out.println("writing " + Integer.toString(b, 2) + " to address "
				+ address);
		ParallelPort.writeOneByte(address, b);
		for (int i = 0; i < 10000; i++)
			;
	}

	/**
	 * Writes the least-significant byte to the first output shift register, and
	 * writes the next least significant byte to the second output shift
	 * register. The third byte is also written to the third register, although
	 * this shift register is currently unused in the physical 6jet controller.
	 * 
	 * @param value
	 *            The value to write
	 */
	public void shiftOut(int value) {
		// FIXME: this doesn't enable pin 6, which enables the 24-volt power
		// supplies used for the valves
		synchronized (writeLock) {
			int byte1 = value & 0xFF;
			int byte2 = (value >> 8) & 0xFF;
			int byte3 = (value >> 16) & 0xFF;
			write(powerStatus);
			for (int i = 0; i < 8; i++) {
				int b1 = byte1 & 1;
				int b2 = byte2 & 1;
				int b3 = byte3 & 1;
				int data = b1 | (b2 << 1) | (b3 << 2);
				write(data | powerStatus);
				write(data | data4 | powerStatus);
				write(data | powerStatus);
				byte1 >>= 1;
				byte2 >>= 1;
				byte3 >>= 1;
			}
			write(powerStatus);
			write(data5 | powerStatus);
			write(powerStatus);
		}
	}

	public void setJetState(int jet, boolean state) {
		if (jet > 23)
			throw new RuntimeException("Invalid jet number " + jet
					+ ", only jets 0 through 23 are allowed");
		int vp = 1 << jet;
		int vn = 0xFFFFFFFF ^ vp;
		if (state)
			jetState |= vp;
		else
			jetState &= vn;
	}

	public void init() {
		powerStateThread.start();
	}

	public boolean getJetState(int jet) {
		if (jet > 23)
			throw new RuntimeException("Invalid jet number " + jet
					+ ", only jets 0 through 23 are allowed");
		return ((jetState >> jet) & 1) != 0;
	}

}
