package com.ornach.nobobutton;


import android.annotation.TargetApi;
import android.content.res.ColorStateList;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.RippleDrawable;
import android.graphics.drawable.StateListDrawable;
import android.os.Build;
import android.view.View;

import androidx.annotation.ColorInt;


public class DrawableHelper {

	public static final int SHAPE_RECTANGLE = 101;
	public static final int SHAPE_OVAL = 102;


	Builder mBuilder;

	protected DrawableHelper(Builder mBuilder) {
		this.mBuilder = mBuilder;
	}

	protected static class Builder{
		protected int radius = 0;
		protected int borderWidth = 0;
		@ColorInt protected  int backgroundColor = Color.TRANSPARENT;
		@ColorInt protected int focusColor =  0xFFCCCCCC;
		@ColorInt protected int disabledColor = Color.TRANSPARENT;
		@ColorInt protected int borderColor = 0;
		protected boolean isEnabled = true;
		private int shape =101;

		public int getRadius() {
			return radius;
		}

		public Builder setRadius(int radius) {
			this.radius = radius;
			return this;
		}

		public int getBorderWidth() {
			return borderWidth;
		}

		public Builder setBorderWidth(int borderWidth) {
			this.borderWidth = borderWidth;
			return this;
		}

		public int getBackgroundColor() {
			return backgroundColor;
		}

		public Builder setBackgroundColor(int backgroundColor) {
			this.backgroundColor = backgroundColor;
			return this;
		}

		public int getFocusColor() {
			return focusColor;
		}

		public Builder setFocusColor(int focusColor) {
			this.focusColor = focusColor;
			return this;
		}

		public int getDisabledColor() {
			return disabledColor;
		}

		public Builder setDisabledColor(int disabledColor) {
			this.disabledColor = disabledColor;
			return this;
		}

		public int getBorderColor() {
			return borderColor;

		}

		public Builder setBorderColor(int borderColor) {
			this.borderColor = borderColor;
			return this;
		}

		public boolean isEnabled() {
			return isEnabled;
		}

		public Builder setEnabled(boolean enabled) {
			isEnabled = enabled;
			return this;
		}

		public int getShape() {
			return shape;
		}

		public Builder setShape(int shape) {
			this.shape = shape;
			return this;
		}

		public DrawableHelper build(){
			return new DrawableHelper(this);
		}

	}

	private Drawable setupBackground() {

		Drawable drawable = null;

		// Default Drawable
		GradientDrawable defaultDrawable = new GradientDrawable();
		defaultDrawable.setCornerRadius(mBuilder.radius);
		defaultDrawable.setColor(mBuilder.backgroundColor);
		//defaultDrawable.setShape(getShape());

		//Focus Drawable
		GradientDrawable focusDrawable = new GradientDrawable();
		focusDrawable.setCornerRadius(mBuilder.radius);
		focusDrawable.setColor(mBuilder.focusColor);
		//focusDrawable.setShape(getShape());

		// Disabled Drawable
		GradientDrawable disabledDrawable = new GradientDrawable();
		disabledDrawable.setCornerRadius(mBuilder.radius);
		disabledDrawable.setColor(mBuilder.disabledColor);
		//disabledDrawable.setShape(getShape());
		//disabledDrawable.setStroke(mBorderWidth, mDisabledBorderColor);


		if (mBuilder.borderColor != 0 && mBuilder.borderWidth > 0) {
			defaultDrawable.setStroke(mBuilder.borderWidth, mBuilder.borderColor);
		}


		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
			drawable = getRippleDrawable(defaultDrawable, focusDrawable, disabledDrawable);
		} else {
			StateListDrawable states = new StateListDrawable();

			// Focus/Pressed Drawable
			GradientDrawable drawable2 = new GradientDrawable();
			drawable2.setCornerRadius(mBuilder.radius);
			drawable2.setColor(mBuilder.focusColor);

			if (mBuilder.focusColor != 0) {
				states.addState(new int[]{android.R.attr.state_pressed}, drawable2);
				states.addState(new int[]{android.R.attr.state_focused}, drawable2);
				states.addState(new int[]{-android.R.attr.state_enabled}, disabledDrawable);
			}

			states.addState(new int[]{}, defaultDrawable);

			drawable = states;
			/*if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) {
				this.setBackgroundDrawable(states);
			} else {
				this.setBackground(states);
			}*/
		}

		return drawable;

	}


	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	private Drawable getRippleDrawable(Drawable defaultDrawable, Drawable focusDrawable, Drawable disabledDrawable) {
		if (!mBuilder.isEnabled) {
			return disabledDrawable;
		} else {
			return new RippleDrawable(ColorStateList.valueOf(mBuilder.focusColor), defaultDrawable, focusDrawable);
		}

	}

	public void setBackground(View view) {

		Drawable drawable = setupBackground();

		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) {
			view.setBackgroundDrawable(drawable);
		} else {
			view.setBackground(drawable);
		}
	}

	public void setBackground(View view, boolean isClickable){
		Drawable drawable = null;

		if (isClickable){
			drawable = setupBackground();
		}else{
			GradientDrawable gradientDrawable = new GradientDrawable();
			gradientDrawable.setCornerRadius(mBuilder.radius);
			gradientDrawable.setColor(mBuilder.backgroundColor);

			//Log.e("TAG", mBuilder.borderColor+"  "+ mBuilder.borderWidth);

			if (mBuilder.borderColor != 0 && mBuilder.borderWidth > 0) {
				//Log.e("TAG", "dksajf");
				gradientDrawable.setStroke(mBuilder.borderWidth, mBuilder.borderColor);
			}

			gradientDrawable.setShape(getShape());

			drawable = gradientDrawable;

		}


		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) {
			view.setBackgroundDrawable(drawable);
		} else {
			view.setBackground(drawable);
		}
	}


	private int getShape() {
		switch (mBuilder.shape) {
			case SHAPE_OVAL:
				return GradientDrawable.OVAL;
			default:
				return GradientDrawable.RECTANGLE;

		}
	}
}
